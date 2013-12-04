;; add to load path
;; (setq load-path (cons "~/emacs.d/elisp/" load-path))
;;
;; (setq path "/bin:/usr/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/local/mysql/bin")
;; (setenv "PATH" path)

(set-register ?p '(file . "~/Dropbox/org/personal.org"))

(if (eq system-type 'darwin)
    (set-default-font "-apple-Andale_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

(modify-frame-parameters nil '((wait-for-wm . nil)))

;;
;; this works load the solarized theme for emacs 24. no need
;; to load color-theme. M-x load-theme solarized-dark
;;
(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-solarized-20111121/")
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/elpa/color-theme-solarized-20111121/")

;; turn off hl line mode from esk
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

;;
;; start org mode config
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-export-html-xml-declaration
      (quote (("html" . "")
              ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
              ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

;; strike thru for DONE tasks
;;(setq org-fontify-done-headline t)
;;(set-face-attribute ‘org-done nil :strike-through t)
;;(set-face-attribute ‘org-headline-done nil :strike-through t)

;;
;; fontification for whatever language you’re using. This is
;; especially nice when you open an editing buffer with Ctrl+c ' to
;; insert 
;; code into the #+begin_src ... #+end_src area.
;;
(setq org-src-fontify-natively t)
;; end org mode config
;;

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(defun my-add-path (path-element)
  "Add the specified path element to the Emacs PATH"
  (interactive "Enter directory to be added to path: ")
  (if (file-directory-p path-element)
      (setenv "PATH"
              (concat (expand-file-name path-element)
                      path-separator (getenv "PATH")))))

(defun prelude-open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun uwb ()
  "Untabifies the whole buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun twb ()
  "Tabifies the whole buffer"
  (interactive)
  (tabify (point-min) (point-max)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

(defun shell-current-directory ()
  "Opens a shell in the current directory"
  (interactive)
  (shell (concat "shell-" default-directory "-shell" )))
(global-set-key (kbd "C-c s") 'shell-current-directory)

;; (defun da-match-paren (arg)
;;   "Go to the matching paren if on a paren."
;;   (interactive "p")
;;   (cond ((and mark-active (looking-at "\\s\(")) (forward-list 1))
;;         ((and mark-active (looking-back "\\s\)")) (backward-list 1))
;;         ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         ))
;; (global-set-key (kbd "M-[") 'da-match-paren)

;;
;; (defun delete-this-buffer-and-file ()
;;   "Removes file connected to current buffer and kills buffer."
;;   (interactive)
;;   (let ((filename (buffer-file-name))
;;         (buffer (current-buffer))
;;         (name (buffer-name)))
;;     (if (not (and filename (file-exists-p filename)))
;;         (error "Buffer '%s' is not visiting a file!" name)
;;       (when (yes-or-no-p "Are you sure you want to remove this file? ")
;;         (delete-file filename)
;;         (kill-buffer buffer)
;;         (message "File '%s' successfully removed" filename)))))
;; (global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)
;;

(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in
region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (and transient-mark-mode mark-active)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))
(global-set-key (kbd "C-7") 'comment-or-uncomment-current-line-or-region)

(defun insert-date()
  "Insert a time-stamp according to locale's date and time
format."
  (interactive)
  (insert (format-time-string "%a, %e %b %Y, %k:%M" (current-time))))
(global-set-key "\C-cd" 'insert-date)

(defun insert-html-break()
  "Insert a org mode html br"
  (interactive)
  (insert "#+BEGIN_HTML<br><br>#+END_HTML"))
(global-set-key "\C-c w" 'insert-html-break)

(defun find-nearest-color (color &optional use-hsv)
  "Finds the nearest color by RGB distance to `color'. If called
with a universal argument (or if `use-hsv' is set) use HSV
instead of RGB. Runs \\[list-colors-display] after setting
`list-colors-sort'"
  (interactive "sColor: \nP")
  (let ((list-colors-sort `(,(if (or use-hsv current-prefix-arg)
                                'hsv-dist
                               'rgb-dist) . ,color)))
    (if (color-defined-p color)
        (list-colors-display)
      (error "The color \"%s\" does not exist." color))))

(defun find-nearest-color-at-point (pt)
  "Finds the nearest color at point `pt'. If called
interactively, `pt' is the value immediately under `point'."
  (interactive "d")
  (find-nearest-color (with-syntax-table (copy-syntax-table (syntax-table))
                        ;; turn `#' into a word constituent to help
                        ;; `thing-at-point' find HTML color codes.
                        (modify-syntax-entry ?# "w")
                        (thing-at-point 'word))))

(defun my-shell-hook()
  (local-set-key (quote [(return)]) (quote newline))
  (local-set-key (quote [(control return)]) (quote comint-send-input))
  (local-set-key (kbd "M-i")  'my-unindent)
  (local-set-key (kbd "C-i")  'indent-or-complete))
(add-hook 'shell-mode-hook 'my-shell-hook)

(defun visit-term-buffer()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))
(global-set-key (kbd "C-c t") 'visit-term-buffer)

;; always take me to the previous window, no matter how many windows
;; are currently present.
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun google()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))
;;(global-set-key (kbd "C-c g") 'google)

(defun youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search YouTube: "))))))
;;(global-set-key (kbd "C-c y") 'youtube)



