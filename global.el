;; visible bell
(setq visible-bell nil)

;; have emacs scroll line-by-line
(setq scroll-step 1)

;; cursor does not blink
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; indentation when hit return
(electric-indent-mode 1)
;;(define-key global-map (kbd "RET") 'newline-and-indent)
;;(setq-default indent-tabs-mode t)

;; Auto revert mode looks for changes to files, and updates them for
;; you. With these settings, dired buffers are also updated. The last
;; setting makes sure that you're not alerted every time this happens.
;; Which is every time you save something.
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; we don't need to see the brag screen
(setq inhibit-splash-screen t)

;; stop creating backup files
(setq make-backup-files nil)

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
(fset 'yes-or-no-p 'y-or-n-p)

;; start in full-screen mode
(if (fboundp 'ns-toggle-fullscreen)
    (ns-toggle-fullscreen))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; Here’s how we can enable spell-checking in source code comments:
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'global)
