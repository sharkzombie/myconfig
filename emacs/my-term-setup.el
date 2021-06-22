
;; New frames are created by emacsclient displaying most recent buffer
;; by default, only after the frame is created it switches to the
;; *scratch* buffer. That causes our code to record the last buffer as
;; belonging to newly created frame.
;;
;; Fix this by switching to *scratch* buffer before frame creation
;;
;; In addition the initial frame buffer flickers briefly
;; use inhibit-redisplay to fix it, there seem to be no ill effects

(defadvice server-create-tty-frame (around create-frames-with-scratch-buffer activate)
  (let ((inhibit-redisplay t)) 
    (with-current-buffer (get-buffer-create "*scratch*")
      (setq ad-return-value ad-do-it))))


(defadvice server-create-window-system-frame (around create-frames-with-scratch-buffer activate)
 (with-current-buffer (get-buffer-create "*scratch*")
   (setq ad-return-value ad-do-it)))

(unless (fboundp 'window-system)
  (defun window-system ()
    nil))

(defun mygetenv (var &optional frame)
  (if (>= emacs-major-version 23)
      (getenv var frame)
    (getenv var)))

(defun get-input-decode-map ()
  (cond ((boundp 'input-decode-map) input-decode-map) ;; emacs 23
        ((boundp 'local-function-key-map) local-function-key-map) ;; emacs 22
        (t function-key-map)))  ;; emacs 21

(defun get-local-function-key-map ()
  (cond ((boundp 'local-function-key-map) local-function-key-map) ;; emacs 22
        (t function-key-map)))

(defun change-modifier (escape-seq 2nd-to-last-char)
  (let ((len (length escape-seq)))
    (concat (substring escape-seq 0 (- len 2))
            (vector 2nd-to-last-char)
            (vector (aref escape-seq (1- len))))))

(defun my-define-tty-key (key command)
  (define-key (get-input-decode-map) key command))

(defun define-tty-key-with-modifiers (key normal shift)
  (let ((s-key (intern (format "S-%s" key)))
        (m-key (intern (format "M-%s" key)))
        (s-m-key (intern (format "S-M-%s" key)))
        (c-key (intern (format "C-%s" key)))
        (c-m-key (intern (format "C-M-%s" key)))
        (c-s-key (intern (format "C-S-%s" key)))
        (c-s-m-key (intern (format "C-S-M-%s" key))))
    (my-define-tty-key normal `[,key])
    (my-define-tty-key (change-modifier shift ?2) `[,s-key])
    (my-define-tty-key (change-modifier shift ?3) `[,m-key])
    (my-define-tty-key (change-modifier shift ?4) `[,s-m-key])
    (my-define-tty-key (change-modifier shift ?5) `[,c-key])
    (my-define-tty-key (change-modifier shift ?6) `[,c-s-key])
    (my-define-tty-key (change-modifier shift ?7) `[,c-m-key])
    (my-define-tty-key (change-modifier shift ?8) `[,c-s-m-key])))

(defun fix-keys ()
  (unless 
      (or
       ;; (>= emacs-major-version 23)
       (member (window-system) '(x)))
    ;; make shift tab [S-tab] instead of [backtab] rebinding of one
    ;; key to another need to be done in local-function-keymap
    (define-key (get-local-function-key-map) [backtab] [S-tab])
    ;; arrow keys
    (define-tty-key-with-modifiers 'up    "\e[A" "\e[1;?A")
    (define-tty-key-with-modifiers 'down  "\e[B" "\e[1;?B")
    (define-tty-key-with-modifiers 'right "\e[C" "\e[1;?C")
    (define-tty-key-with-modifiers 'left  "\e[D" "\e[1;?D")


    (define-tty-key-with-modifiers 'insert  "\e[2~" "\e[2;?~")
    ;; delete is tricky. The regular Delete key should be deletechar
    ;; but the modified key should be S-delete M-delete etc
    (define-tty-key-with-modifiers 'delete  "\e[3~" "\e[3;?~")
    (my-define-tty-key "\e[3~" [deletechar])
    ;; home and end need 2 variants, one is what xterm uses and one is what 
    ;; screen uses
    (define-tty-key-with-modifiers 'home  "\e[H" "\e[1;?H")
    (define-tty-key-with-modifiers 'end  "\e[F" "\e[1;?F")
    (define-tty-key-with-modifiers 'home  "\e[1~" "\e[1;?H")
    ;; somehow in emacs 26.3 4~ is mapped to <select>, so remove it
    (define-key input-decode-map "\e[4~" nil)
    
    (define-tty-key-with-modifiers 'end  "\e[4~" "\e[1;?F")
    (define-tty-key-with-modifiers 'prior  "\e[5~" "\e[5;?~")
    (define-tty-key-with-modifiers 'next  "\e[6~" "\e[6;?~")
    ;; old style xterm function keys (so that they do not start with ESC O
    (define-tty-key-with-modifiers 'f1 "\e[11~" "\e[11;2~")
    (define-tty-key-with-modifiers 'f2 "\e[12~" "\e[12;2~")
    (define-tty-key-with-modifiers 'f3 "\e[13~" "\e[13;2~")
    (define-tty-key-with-modifiers 'f4 "\e[14~" "\e[14;2~")
    (define-tty-key-with-modifiers 'f5 "\e[15~" "\e[15;2~")
    (define-tty-key-with-modifiers 'f6 "\e[17~" "\e[17;2~")
    (define-tty-key-with-modifiers 'f7 "\e[18~" "\e[18;2~")
    (define-tty-key-with-modifiers 'f8 "\e[19~" "\e[19;2~")
    (define-tty-key-with-modifiers 'f9 "\e[20~" "\e[20;2~")
    (define-tty-key-with-modifiers 'f10 "\e[21~" "\e[21;2~")
    (define-tty-key-with-modifiers 'f11 "\e[23~" "\e[23;2~")
    (define-tty-key-with-modifiers 'f12 "\e[24~" "\e[24;2~")
    ;; ESC O really sucks in VI mode, therefore switch App keys mode off
    ;; and delete that map
    ;; no need to emacs 24+ already does this
    ;; actually it does not
    (send-string-to-terminal "\e[?1l")
    (define-key (get-input-decode-map) "\eO" nil)
    (set-input-meta-mode 'encoded)
    (when (eql 0 (nth 2 (current-input-mode)))
      (set-input-meta-mode t))))


(defun my-tty-setup-hook ()
  (set-input-meta-mode 'encoded)
  (when (eql 0 (nth 2 (current-input-mode)))
    (set-input-meta-mode t))
  (send-string-to-terminal "\e[?1l")
    ;;(fix-keys)
  )

(add-hook 'tty-setup-hook 'my-tty-setup-hook)

(fix-keys) ; need this for emacs 22
(menu-bar-mode 0)

;; functions for screwing around with the terminal
(provide 'my-term-setup)
