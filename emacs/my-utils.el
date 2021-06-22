
(defun require-if-available (&rest args)
  "require symbols, load-library strings, fail silently if 
   some aren't available"
  (let (lib)
    (condition-case err
        (mapc (lambda (e)
                (setq lib e)
                (message "Loading %S" lib)
                (cond
                 ((stringp e) (load-library e))
                 ((symbolp e) (require e))))
	      args)
      (quit (progn (message "Error while loading extension %S: %S"
                            lib err)
		   nil))
      (error  (progn (message "Error while loading extension %S: %S"
                              lib err)
		     nil)))))

(defun frame-buffer-list ()
  "Return buffer list on selected frame"
  (let ((buffers '()))
    (dolist (window (window-list nil 'no-minibuff) buffers)
      (push (window-buffer window) buffers))))

(defun remove-from-list (var element)
  (do ((head (symbol-value var) (cdr head))
        (prev nil head))
      ((null head) (symbol-value var))
    (when (eq (car head) element)
      (if prev (setcdr prev (cdr head))
        (set var (cdr head))))))

(provide 'my-utils)
