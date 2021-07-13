
(require 'evil)

(evil-mode)


(defun my-exec-key-in-emacs (&optional prefix)
  "Execute last command key in emacs state"
  (interactive "P")
  (let ((key last-command-event)
        com) 
    (setq
     unread-command-events
     (let ((new-events
            (cond ((eventp key) (list key))
                  ((listp key) key)
                  ((sequencep key)
                   (listify-key-sequence key))
                  (t (error
                      "exec-key-in-emacs: Invalid argument, %S"
                      key)))))
       (if (not (eventp nil))
           (setq new-events (delq nil new-events)))
       (append new-events unread-command-events)))
    (condition-case nil
        (evil-with-state emacs 
          (setq com
                (key-binding (setq key (read-key-sequence nil))))
          (while (vectorp com) (setq com (key-binding com)))
          ;; this-command, last-command-char, last-command-event
         (setq this-command com)
         (setq last-command-event key)
          (when (commandp com)
            (setq prefix-arg (or prefix-arg prefix))
            (command-execute com)))
      (quit (ding))
      (error (beep 1)))))

;; overriding this
(defun evil-esc-mapping ()
  (prog1 [escape]
    (when defining-kbd-macro
      (end-kbd-macro)
      (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
      (start-kbd-macro t t))))

(defun evil-esc (map)
  "Translate \\e to 'escape if no further event arrives.
This function is used to translate a \\e event either to 'escape
or to the standard ESC prefix translation map. If \\e arrives,
this function waits for `evil-esc-delay' seconds for another
event. If no other event arrives, the event is translated to
'escape, otherwise it is translated to the standard ESC prefix
map stored in `input-decode-map'. If `evil-inhibit-esc' is
non-nil or if evil is in emacs state, the event is always
translated to the ESC prefix.

The translation to 'escape happens only if the current command
has indeed been triggered by \\e. In other words, this will only
happen when the keymap is accessed from `read-key-sequence'. In
particular, if it is access from `define-key' the returned
mapping will always be the ESC prefix map."
  (let ((ret
	 (if (and (not evil-inhibit-esc)
		  (or evil-local-mode (evil-ex-p))
		  (not (evil-emacs-state-p))
		  (equal (this-single-command-keys) [?\e]))
	     (let ((event (read-event nil nil evil-esc-delay)))
	       ;; (log-sexp event)
	       (cond ((null event)
		      ;; (message "Returning [escape]")
		      (evil-esc-mapping))
		     ((or (and (>= event ?a)
			       (<= event ?z))
			  (and (>= event ?A)
			       (<= event ?Z)))
		      (setq unread-command-events
			    (append unread-command-events (list event)))
		      ;; (message "Returning [escape] with unread %c" event)
		      (evil-esc-mapping))
		     (t
		      ;; (message "Returning original map with unread %c" event)
		      (setq unread-command-events
			    (append unread-command-events (list event)))
		      map)))
	   map)))
    ;;(log-sexp ret)
    ret))


(defun my-quit-restore-window ()
  "Calls quit-restore-window with 'kill argument"
  (interactive)
  (quit-restore-window nil 'kill))

(evil-define-key '(normal motion insert) 'global (kbd "TAB") 'my-exec-key-in-emacs)

(defvar evil-Zopen-map (make-sparse-keymap)
  "Keymap for ZO command in Evil mode")

;; z bindings
(evil-define-key '(normal motion) 'global "zo" 'find-file)
(evil-define-key '(normal motion) 'global "zk" 'my-quit-restore-window)
(evil-define-key '(normal motion) 'global "zb" 'bury-buffer)
(evil-define-key '(normal motion) 'global "zE" 'eval-expression)
;; s bindings 
(evil-define-key '(normal motion) 'global "s" nil)
(evil-define-key '(normal motion) 'global "sg" 'grep)
(evil-define-key '(normal motion) 'global "sp" 'describe-text-properties)
;; st bindings
(evil-define-key '(normal motion) 'global "stt" 'toggle-truncate-lines)
(evil-define-key '(normal motion) 'global "std" 'toggle-debug-on-error)
(evil-define-key '(normal motion) 'global "sth" 'hl-line-mode)
(evil-define-key '(normal motion) 'global "stc" 'toggle-case-fold-search)
(evil-define-key '(normal motion) 'global "str" 'refill-mode)
(when (require-if-available 'show-point-mode)
  (evil-define-key '(normal motion) 'global "stp" 'show-point-mode))

;; ; map
(evil-define-key '(motion normal visual) 'global ";" nil)
(evil-define-key '(normal visual) 'global ";c" 'comment-dwim)
(evil-define-key '(motion) 'global ";b" 'switch-to-buffer)

;; z key map setup
;;(defvar evil-z-map (make-sparse-keymap) "Keymap for z key")
;; note that this override evil folding map
;;(define-key evil-normal-state-map "z" evil-z-map)
;;(define-key evil-motion-state-map "z" evil-z-map)
;; z map commands
;; z key map bindings
;;(define-key evil-z-map "o" 'find-file)
;;(define-key evil-z-map "k" 'my-zk)
;;(define-key evil-z-map "b" 'bury-buffer)

;; s map setup
;; (defvar evil-s-map (make-sparse-keymap)
;;  "Keymap for s dispatch command in Evil mode")
;; this overrides evil-substitute command
;;(define-key evil-normal-state-map "s" evil-s-map)
;;(define-key evil-motion-state-map "s" evil-s-map)
;; s map bindings 
;;(define-key evil-s-map "g" 'grep)
;;(define-key evil-s-map "p" 'describe-text-properties)

;; (defvar evil-S-visual-map (make-sparse-keymap)
;;  "Keymap for S command in Evil visual mode")

;; st (toggle) map setup
;; (defvar evil-toggle-map (make-sparse-keymap)
;;   "Keymap for toggling various things command in Evil mode")
;; (define-key evil-s-map "t" evil-toggle-map)
;; st (toggle) map bindings
;; (define-key evil-toggle-map "t" 'toggle-truncate-lines)
;; (define-key evil-toggle-map "d" 'toggle-debug-on-error)
;; (define-key evil-toggle-map "h" 'hl-line-mode)
;; (define-key evil-toggle-map "g" 'toggle-gud-popups)
;; (define-key evil-toggle-map "r" 'refill-mode)
;; (define-key evil-toggle-map "c" 'toggle-case-fold-search)
;; (when (require-if-available 'show-point-mode)
;;       (define-key evil-toggle-map "p" 'show-point-mode))


;; evil ; key map
;; (defvar evil-semicolon-map (make-sparse-keymap) "Keymap for the ; key in Evil")
;; (define-key evil-normal-state-map)
;; (define-key evil-normal-state-map ";c" 'comment-dwim)
;; (define-key evil-visual-state-map ";c" 'comment-dwim)


(defun my-can-modify-text ()
  "Verifies if are able to modify text at pointp"
  (let (ok) 
    (condition-case nil 
        (with-silent-modifications 
          (let ((inhibit-read-only nil)) 
            (insert-and-inherit ?\ ) 
            (delete-region (1- (point)) (point)))
          t)
      (error nil))))

(defadvice evil-insert-state (before dont-insert-on-readonly-text activate)
  (when (and (not (evil-insert-state-p))
             (or (not (numberp arg))
                 (not (minusp arg)))
             (not (my-can-modify-text)))
    (error "Can't enter insert state on read only text")))

;; I like my C-w to do same thing in insert mode
(evil-define-key '(insert) 'global "\C-w" evil-window-map)

(setq evil-symbol-word-search t)

(provide 'my-evil-setup)
