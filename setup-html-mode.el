;; In html-mode, forward/backward-paragraph is infuriatingly slow.

(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map
       [remap forward-paragraph] 'skip-to-next-blank-line)

     (define-key html-mode-map
       [remap backward-paragraph] 'skip-to-previous-blank-line)))

;; I use them a lot for quick navigation. In html-mode, they are
;; anything but quick. Defining paragraphs in Emacs is black magic,
;; and I'm not sure it's a good idea to change that in case something
;; else relies on its erratic behavior. Instead I just remap the
;; commands to my home brewed skip-to-next/previous-blank-line. Ahh,
;; speedy and predictable navigation once more.
