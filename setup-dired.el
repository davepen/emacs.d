;; By installing M-x package-install dired-details and using this
;; snippet, we hide all the unnecessary ls-details. That rare occasion
;; where you actually need that information, you can show it with) and
;; hide again with (.
;; Make dired less verbose
;;
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

(require 'dired-x)

;; In dired, M-> and M-< never take me where I want to go. Instead of
;; taking me to the very beginning or very end, they now take me to
;; the first or last file
;;
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

