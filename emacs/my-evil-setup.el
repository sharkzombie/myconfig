
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

;; overriding this too
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

;; overriding this, the only change is comment out of (when evil-ex-p) part
;; which causes c key to change entire line in C-f ex window
(defun evil-operator-range (&optional return-type)
  "Read a motion from the keyboard and return its buffer positions.
The return value is a list (BEG END), or (BEG END TYPE) if
RETURN-TYPE is non-nil."
  (let* ((evil-ex-p (and (not (minibufferp)) (evil-ex-p)))
         (motion (or evil-operator-range-motion
                     ;; (when evil-ex-p 'evil-line)
                     ))
         (type evil-operator-range-type)
         (range (evil-range (point) (point)))
         command count)
    (setq evil-this-type-modified nil)
    (evil-save-echo-area
      (cond
       ;; Ex mode
       ((and evil-ex-p evil-ex-range)
        (setq range evil-ex-range))
       ;; Visual selection
       ((and (not evil-ex-p) (evil-visual-state-p))
        (setq range (evil-visual-range)))
       ;; active region
       ((and (not evil-ex-p) (region-active-p))
        (setq range (evil-range (region-beginning)
                                (region-end)
                                (or evil-this-type 'exclusive))))
       (t
        ;; motion
        (evil-save-state
          (unless motion
            (evil-change-state 'operator)
            ;; Make linewise operator shortcuts. E.g., "d" yields the
            ;; shortcut "dd", and "g?" yields shortcuts "g??" and "g?g?".
            (let ((keys (nth 2 (evil-extract-count (this-command-keys)))))
              (setq keys (listify-key-sequence keys))
              (dotimes (var (length keys))
                (define-key evil-operator-shortcut-map
                  (vconcat (nthcdr var keys)) 'evil-line-or-visual-line)))
            ;; read motion from keyboard
            (setq command (evil-read-motion motion)
                  motion (nth 0 command)
                  count (nth 1 command)
                  type (or type (nth 2 command))))
          (cond
           ((eq motion #'undefined)
            (setq range (if return-type '(nil nil nil) '(nil nil))
                  motion nil))
           ((or (null motion)           ; keyboard-quit
                (evil-get-command-property motion :suppress-operator))
            (when (fboundp 'evil-repeat-abort)
              (evil-repeat-abort))
            (setq quit-flag t
                  motion nil))
           (evil-repeat-count
            (setq count evil-repeat-count
                  ;; only the first operator's count is overwritten
                  evil-repeat-count nil))
           ((or count current-prefix-arg)
            ;; multiply operator count and motion count together
            (setq count
                  (* (prefix-numeric-value count)
                     (prefix-numeric-value current-prefix-arg)))))
          (when motion
            (let ((evil-state 'operator)
                  mark-active)
              ;; calculate motion range
              (setq range (evil-motion-range
                           motion
                           count
                           type))))
          ;; update global variables
          (setq evil-this-motion motion
                evil-this-motion-count count
                type (evil-type range type)
                evil-this-type type))))
      (when (evil-range-p range)
        (unless (or (null type) (eq (evil-type range) type))
          (evil-contract-range range)
          (evil-set-type range type)
          (evil-expand-range range))
        (evil-set-range-properties range nil)
        (unless return-type
          (evil-set-type range nil))
        (setq evil-operator-range-beginning (evil-range-beginning range)
              evil-operator-range-end (evil-range-end range)
              evil-operator-range-type (evil-type range)))
      range)))


(defun my-quit-restore-window ()
  "Calls quit-restore-window with 'kill argument"
  (interactive)
  (quit-restore-window nil 'kill))

(evil-define-minor-mode-key '(normal insert) 'cua-mode (kbd "C-v") 'cua-paste)
(evil-define-key '(motion) 'global (kbd "C-\\") 'evil-visual-block)
(evil-define-key '(normal motion insert) 'global (kbd "TAB") 'my-exec-key-in-emacs)
;; get rid of the Ctrl-R for redo and assign C-
(evil-define-key '(normal motion) 'global (kbd "C-r") nil)
(evil-define-key '(normal motion) 'global (kbd "C-M-z") 'evil-redo)

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
(evil-define-key '(normal motion) 'global "stp" 'show-point-mode)

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
