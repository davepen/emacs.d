(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key [(shift return)] 'smart-open-line)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control shift return)] 'smart-open-line-above)

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))
;; Rebind C-S-Backspace.
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


(require 'whitespace)
(setq whitespace-line-column 80)       ;; limit line length
(setq whitespace-style '(lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;;The above snippet will enable whitespace-mode only in major modes
;;for programming. If you want to enable whitespace-mode everywhere
;;you might want to do this instead:
;;(global-whitespace-mode +1)


(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations. This
function should be added to the hooks of major modes for
programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
;;(add-hook 'prog-mode-hook 'font-lock-comment-annotations)


(defun url-humanify ()
  "Take the URL at point and make it human readable."
  (interactive)
  (let* ((area (bounds-of-thing-at-point 'url))
         (num-params  (count-occurances-in-region "&" (car area) (cdr area)))
         (i 0))
    (beginning-of-thing 'url)
    (when (search-forward "?" (cdr area) t nil)
      (insert "\n  ")
      (while (< i num-params)
        (search-forward "&" nil t nil)
        (insert "\n  ")
        (save-excursion
          (previous-line)
          (beginning-of-line)
          (let ((start (search-forward "="))
                (end (search-forward "&")))
            (url-decode-region start end)))
        (setq i (+ i 1))))))

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun count-occurances-in-region (needle start end)
  (save-excursion
    (let ((found 0))
      (goto-char start)
      (while (search-forward needle end t nil)
        (setq found (+ found 1)))
      found)))
