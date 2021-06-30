(require 'compile)

(when (require-if-available 'ansi-color)
  (defun my/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  
  (add-hook 'compilation-filter-hook
	    #'my/colorize-compilation))

(evil-define-key '(motion normal insert visual) 'global "\M-gc" 'compile)


(provide 'my-compile)
