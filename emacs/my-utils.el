
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

(defun my-in-string-p ()
  (ignore-errors (in-string-p)))

(defmacro log-sexp (&rest exprs)
  "Log each expression literally and their evaluated form.
For example (log-expr a b) will log 'a=123 b=321'
" 
  (let* ((first t)
         (format
          (with-output-to-string
            (dolist (e exprs)
              (if (not first) (princ " ") (setq first nil))
	      (princ e)
	      (unless (stringp e)
		(princ "=")
		(princ "%S"))))))
    `(message ,format ,@exprs)))

(defalias 'log-expr 'log-sexp)

(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (body)
    (condition-case err
                    (progn ,@body)
      (error nil))))

(defun mm/should-mode-be-enabled-p (arg mode)
  "Utility for functions that enable or toggle a minor mode, that determines if mode should be on based on arg and status"
  (if (null arg) (not mode)
    (plusp arg)))

(provide 'my-utils)
