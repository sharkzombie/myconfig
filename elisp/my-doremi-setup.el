(require 'doremi)
(require 'doremi-cmd)
(require 'doremi-frm)

(defcustom doremi-up-vi-keys '(?k ?h ?n)
  "VI keys to be added to `doremi-up-keys'."
  :type '(repeat (restricted-sexp :match-alternatives (integerp symbolp))) :group 'doremi)

(defcustom doremi-boost-up-vi-keys '(?\C-h)
  "VI keys to be added to `doremi-boost-up-keys'."
  :type '(repeat (restricted-sexp :match-alternatives (integerp symbolp))) :group 'doremi)

(defcustom doremi-down-vi-keys '(?j ?l ?N)
  "VI keys to be added to `doremi-down-keys'."
  :type '(repeat (restricted-sexp :match-alternatives (integerp symbolp))) :group 'doremi)

(defcustom doremi-boost-down-vi-keys '(?\C-l)
  "VI keys to be added to `doremi-boost-down-keys'."
  :type '(repeat (restricted-sexp :match-alternatives (integerp symbolp))) :group 'doremi)

(defmacro mm/with-vi-doremi-keys (reverse &body body)
  `(let ((doremi-up-keys (append doremi-up-keys '(?k)))
         (doremi-down-keys (append doremi-down-keys '(?j))))
     (setq ad-return-value ad-do-it)))

(defvar doremi-reverse-keys nil
  "When non-NIL reverse the `doremi-up-keys' and `doremi-down-keys'")

(defadvice doremi (around use-vi-keys activate)
  "Use jk and nN keys  as next/prev"
  (let ((doremi-up-keys (append doremi-up-keys doremi-up-vi-keys))
        (doremi-down-keys (append doremi-down-keys doremi-down-vi-keys))
        (doremi-boost-up-keys (append doremi-boost-up-keys doremi-boost-up-vi-keys))
        (doremi-boost-down-keys (append doremi-boost-down-keys doremi-boost-down-vi-keys)))
    (when doremi-reverse-keys
      (rotatef doremi-up-keys doremi-down-keys)
      (rotatef doremi-boost-up-keys doremi-boost-down-keys))
    (setq ad-return-value ad-do-it)))

(defadvice doremi-window-height+ (around use-vi-keys activate)
  "Reverse doremi vi keys, if window is "
  (let* ((fw (frame-first-window))
         (doremi-reverse-keys
          (eq (selected-window) fw)))
    (setq ad-return-value ad-do-it)))

(define-key evil-window-map "\C-h" 'doremi-window-height+)
(define-key evil-window-map "H" 'doremi-window-height+)

(provide 'my-doremi-setup)
;;; my-doremi-setup.el ends here

