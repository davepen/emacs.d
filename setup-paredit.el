(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(define-key paredit-mode-map (kbd "M-)")
  'paredit-wrap-round-from-behind)

;; With point in front of a sexp, paredit-wrap-round (bound to M-(),
;; will open a paren in front the the sexp, and place the closing
;; paren at the end of it. That's pretty handy.

;; This snippet does the same, but from the other end. It saves me a
;; C-M-b ever so often. I like it.

(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

;; Like Kototama says in his blogpost, duplicating a line is very
;; useful, but sometimes it leads to invalid sexps. In the blogpost he
;; shows a snippet that will duplicate the sexp after point. I
;; immediately realized I had really been wanting this.

;; The version listed here is a little modified: It will duplicate the
;; sexp you are currently inside, or looking at, or looking behind at.
;; So basically, point can be in any of these positions:

;;  |(my sexp) ;; in front
;;  (my| sexp) ;; inside
;;  (my sexp)| ;; at the end

;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

;; This makes paredit-mode work with delete-selection-mode, replacing
;; its wrapping behavior. If I want to wrap, I'll do it with the
;; paredit-wrap-* commands explicitly.
