(require 'compile)

(when (require-if-available 'ansi-color)
  (defun my/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  
  (add-hook 'compilation-filter-hook
	    #'my/colorize-compilation))

(defvar mm/last-compile-type nil)
(defvar mm/last-slime-system-name nil)

(defun mm/slime-save-some-lisp-buffers ()
  (if (and (featurep 'slime) slime-repl-only-save-lisp-buffers)
      (save-some-buffers t (lambda ()
			     (and (memq major-mode slime-lisp-modes)
				  (not (null buffer-file-name)))))
    (save-some-buffers t)))

(defun my-compile (&optional arg)
  (interactive "P")
  (cond ((or (member major-mode '(lisp-mode slime-repl-mode slime-xref-mode))
             (and (not arg)
                  (eq mm/last-compile-type :lisp)))
         (setq mm/last-compile-type :lisp)
         (mm/slime-save-some-lisp-buffers)
         (let ((system (if (or arg (not mm/last-slime-system-name))
                           (slime-read-system-name)
                         mm/last-slime-system-name)))
           (setq mm/last-slime-system-name system)
           (slime-oos system 'load-op)))
        (t
         (setq mm/last-compile-type nil)
         (if arg
             (call-interactively 'compile)
           (compile compile-command)))))

(global-set-key [(f7)] 'my-compile)
(define-key global-map "\M-gc" 'my-compile)
(evil-define-key '(motion normal insert visual) 'global "\M-gc" 'my-compile)
(evil-define-key '(motion normal insert visual) 'global [(f7)] 'my-compile)


(provide 'my-compile)
