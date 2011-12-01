;; visible bell
(setq visible-bell nil)

;; have emacs scroll line-by-line
(setq scroll-step 1)

;; cursor does not blink
(blink-cursor-mode -1)

;; we don't need to see the brag screen
(setq inhibit-splash-screen t)

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)

(setq c-default-style "bsd"
      c-basic-offset 4)

;; we don't need to see scroll bars, tool bars, or menu bars
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;; replace the yes or no prompt with y or n
;;
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'global)
