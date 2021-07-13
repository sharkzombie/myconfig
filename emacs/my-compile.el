(require 'hl-line)
(require 'compile)

(when (require-if-available 'ansi-color)
  (defun my/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  
  (remove-hook 'compilation-filter-hook
	    #'my/colorize-compilation)
  (add-hook 'compilation-filter-hook
	    #'my/colorize-compilation))


(defun my-after-compilation-mode ()
  (hl-line-mode 1))

(defun my-on-next-error-hook ()
  "Somehow hl-line does not highlight the compilation mode line even with hl-line-sticky-flag, so fix it"
  (with-current-buffer next-error-last-buffer
    (when (and (boundp 'hl-line-mode)
               hl-line-mode)
      (hl-line-highlight))))

(add-hook 'next-error-hook 'my-on-next-error-hook)

(defvar my-last-compile-type nil)
(defvar my-last-slime-system-name nil)

(defun my-slime-save-some-lisp-buffers ()
  (if (and (featurep 'slime) slime-repl-only-save-lisp-buffers)
      (save-some-buffers t (lambda ()
			     (and (memq major-mode slime-lisp-modes)
				  (not (null buffer-file-name)))))
    (save-some-buffers t)))

(defun my-compile (&optional arg)
  (interactive "P")
  (cond ((or (member major-mode '(lisp-mode slime-repl-mode slime-xref-mode))
             (and (not arg)
                  (eq my-last-compile-type :lisp)))
         (setq my-last-compile-type :lisp)
         (my-slime-save-some-lisp-buffers)
         (let ((system (if (or arg (not my-last-slime-system-name))
                           (slime-read-system-name)
                         my-last-slime-system-name)))
           (setq my-last-slime-system-name system)
           (slime-oos system 'load-op)))
        (t
         (setq my-last-compile-type nil)
         (if arg
             (call-interactively 'compile)
           (compile compile-command)))))

(global-set-key [(f7)] 'my-compile)
(define-key global-map "\M-gc" 'my-compile)
(evil-define-key '(motion normal insert visual) 'global "\M-gc" 'my-compile)
(evil-define-key '(motion normal insert visual) 'global [(f7)] 'my-compile)


(provide 'my-compile)
