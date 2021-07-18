(defvar original-line-number-mode (alist-get 'line-number-mode mode-line-position))

(let ((show-point-mode-format
       `((show-point-mode
	  (line-number-mode
	   ((column-number-mode
	     (15 " (%l,%c)(" (:eval (format "%d" (point))) ")")
	     (10 " L%l(" (:eval (format "%d" (point))) ")")))
	   ((column-number-mode
	     (10 " C%c(" (:eval (format "%d" (point))) ")"))))
	  ,original-line-number-mode))))
  (setq mode-line-position
        (assq-delete-all
         'show-point-mode 
         (assq-delete-all 'line-number-mode mode-line-position)))
  (setq mode-line-position
	(append mode-line-position
                show-point-mode-format)))

(define-minor-mode show-point-mode
  "Toggle show-point mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.
"

  ;; initial value, mode-line indicator, and keymap
  nil nil nil
  
  ;; if show-point-mode had been turned on, add update function to
  ;; `post-command-hook'
  )

(define-globalized-minor-mode global-show-point-mode
  show-point-mode (lambda ()
                    (show-point-mode 1)))

(provide 'show-point-mode)

