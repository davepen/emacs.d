(global-set-key (kbd "C-x g") 'webjump)

;; Add Urban Dictionary to webjump
(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Urban Dictionary" .
                [simple-query
                 "www.urbandictionary.com"
                 "http://www.urbandictionary.com/define.php?term="
                 ""])))

(defun jcs-datetime (arg)
  "Without argument: insert date as yyyy-mm-dd
With C-u: insert time
With C-u C-u: insert date and time"
  (interactive "P")
  (cond ((equal arg '(4)) (insert (format-time-string "%T")))
        ((equal arg '(16)) (insert (format-time-string "%Y-%m-%d %T")))
        (t (insert (format-time-string "%Y-%m-%d")))))
