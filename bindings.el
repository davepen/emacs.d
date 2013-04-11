(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c a") 'apropos)
(global-set-key (kbd "C-c v") 'visual-line-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Function to create new functions that look for a specific pattern
;; (defun ffip-create-pattern-file-finder (&rest patterns)
;;   (lexical-let ((patterns patterns))
;;                (lambda ()
;;                  (interactive)
;;                  (let ((ffip-patterns patterns))
;;                    (find-file-in-project)))))

;; ;; Find file in project, with specific patterns
;; (global-unset-key (kbd "C-x C-o"))
;; (global-set-key (kbd "C-x C-o ja")
;;                 (ffip-create-pattern-file-finder "*.java"))
;; (global-set-key (kbd "C-x C-o js")
;;                 (ffip-create-pattern-file-finder "*.js"))
;; (global-set-key (kbd "C-x C-o jp")
;;                 (ffip-create-pattern-file-finder "*.jsp"))

(provide 'bindings)
