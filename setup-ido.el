(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

;; This is a bad idea if your files are prefixed with ~. But if
;; they're not, this keybinding lets you even more quickly reach your
;; home folder when in ido-find-file. It doesn't matter if you're a
;; million directories in, just press ~ to go home.

