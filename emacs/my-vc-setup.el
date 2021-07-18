
(require 'vc)
(require 'vc-dir)
(require 'log-edit)
(require 'log-view)
(require 'diff-mode)
(require 'vc-hg)
(require 'vc-git)
(require 'egg)

;; (defvar vc-diff-switches-history nil
;;   "History for vc-diff command switches")

;; (defadvice vc-diff (around maybe-ignore-whitespace activate)
;;   "With prefix argument 1 ignore whitespace, with prefix argument 2 ask
;; for the diff switches"
;;   (let ((arg current-prefix-arg))
;;     (cond ((null arg)
;;            (setq ad-return-value ad-do-it))
;;           ((member arg '(1 2))
;;            (setq current-prefix-arg nil)
;;            (let ((vc-diff-switches "-w")
;;                  (vc-git-diff-switches "-w")
;;                  (vc-hg-diff-switches "-w")
;;                  (current-prefix-arg nil))
;;              (setq ad-return-value (vc-diff (equal arg 2) t))))
;;           ((member arg '(3 4 (16)))
;;            (setq current-prefix-arg nil)
;;            (let* ((switches
;;                    (read-from-minibuffer "Diff switches: " nil nil nil 
;;                                          'vc-diff-switches-history)))
;;              (if switches
;;                  (let ((vc-diff-switches switches)
;;                        (vc-git-diff-switches switches)
;;                        (vc-hg-diff-switches switches))
;;                    (setq ad-return-value (vc-diff (equal arg 4) t)))
;;                (setq ad-return-value (vc-diff (equal arg 4) t)))))
;;           (t (setq ad-return-value ad-do-it)))))

;; (define-key evil-normal-state-map ";v" 'vc-prefix-map)

(dolist (mode '(log-edit-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (add-to-list 'evil-insert-state-modes mode))

(dolist (mode '(vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode
                            diff-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (add-to-list 'evil-normal-state-modes mode))


;; (define-key log-view-mode-map "\C-n" 'log-view-msg-next)
;; (define-key log-view-mode-map "\C-p" 'log-view-msg-prev)
;; (define-key log-view-mode-map (kbd "C-M-n") 'log-view-file-next)
;; (define-key log-view-mode-map (kbd "C-M-p") 'log-view-file-prev)

;; (define-key diff-mode-shared-map "\C-n" 'diff-hunk-next)
;; (define-key diff-mode-shared-map "\C-p" 'diff-hunk-prev)
;; (define-key diff-mode-shared-map (kbd "C-M-n") 'diff-file-next)
;; (define-key diff-mode-shared-map (kbd "C-M-p") 'diff-file-prev)

;;(evil-give-back-keys-in-mode '(vc-dir-mode special-mode) evil-give-back-keys-updown-only)

;;(evil-give-back-keys-in-mode '(vc-git-log-view-mode log-view-mode))
;;(evil-give-back-keys-in-mode '(vc-hg-log-view-mode log-view-mode))
;;(evil-give-back-keys-in-mode '(diff-mode diff-mode-shared))

;; (evil-define-key 'normal diff-mode-map "z" nil)
;; (evil-define-key 'normal diff-mode-map "zk" 'diff-hunk-kill)


;; Egg mode setup
(evil-define-key 'normal egg-minor-mode-map ";v" egg-file-cmd-map)
(evil-define-key 'motion egg-minor-mode-map ";v" egg-file-cmd-map)
(define-key egg-hide-show-map (kbd "TAB") 'egg-section-cmd-toggle-hide-show)
(define-key egg-hide-show-map (kbd "h")
  (lambda (&optional arg)
    (interactive)
    (if (bolp)
        (call-interactively 'egg-section-cmd-toggle-hide-show)
      (call-interactively 'evil-backward-char))))
  
(define-key egg-hide-show-map (kbd " ") 'egg-section-cmd-toggle-hide-show-children)

(dolist (mode '(egg-commit-buffer-mode
                egg-diff-buffer-mode
                egg-status-buffer-mode
                egg-log-buffer-mode
                (egg-file-log-buffer-mode egg-log-buffer-mode)
                (egg-reflog-buffer-mode egg-log-buffer-mode)))
  (let ((modes mode)
        (mode (if (symbolp mode) mode (car mode))))
    (add-to-list (if (eq mode 'egg-commit-buffer-mode)
                     'evil-normal-state-modes
                   'evil-motion-state-modes)
                 mode)
    (remove-from-list 'evil-emacs-state-modes mode)
    (remove-from-list 'evil-insert-state-modes mode)
    (remove-from-list (if (eq mode 'egg-commit-buffer-mode)
                          'evil-motion-state-modes
                        'evil-normal-state-modes)
                      mode)
    ;;(evil-give-back-keys-in-mode modes)
    ;; (evil-define-key 'normal-mode-map ";v" egg-file-cmd-map)
    ))



(dolist (mode '(egg-commit-buffer-mode vc-git-log-edit-mode))
  (remove-from-list 'evil-normal-state-modes mode)
  (add-to-list 'evil-insert-state-modes mode))

;; (defadvice vc-dir (around use-egg-instead-of-git activate)
;;   (let ((backend (vc-responsible-backend dir)))
;;     (if (eq backend 'Git)
;;         (let ((default-directory dir))
;;           (my-egg-status))
;;       (setq ad-return-value ad-do-it))))

;; (defun my-egg-log (&optional arg)
;;   "Call `egg-log' with T parameter so it switches to status buffer"
;;   (interactive)
;;   (call-interactively 'egg-log)
;;   ;; (egg-buffer-hide-section-type :diff)
;;   )

;; (defadvice vc-print-log-internal (around use-egg-instead-of-git activate)
;;   (if (eq backend 'Git) (my-egg-log)
;;     (setq ad-return-value ad-do-it)))

;; (defun my-egg-status (&optional arg)
;;   "Call `egg-status' with T parameter so it switches to status buffer"
;;   (interactive)
;;   (egg-status t t)
;;   ;; (egg-buffer-hide-section-type :diff)
;;   )

;; (define-key egg-file-cmd-map "d" 'my-egg-status)
;; (define-key egg-file-cmd-map "s" 'my-egg-status)

(when (fboundp 'egg-add-log-message)
  (defadvice egg-add-log-message (after evil-go-insert activate)
    (evil-insert-state)))

(when (fboundp 'egg-commit-log-edit)
  (defadvice egg-commit-log-edit (after evil-go-insert activate)
    (evil-insert-state)))


;; redefine these, so that p/n keys (bound to egg-buffer-cmd-next/prev-block)
;; do not stop inside the hidden text
(defun egg-buffer-cmd-next-block (nav-prop)
  "Move to the next block indentified by text property NAV-PROP."
  (let ((done nil)
        (prev (point))) 
    (while (not done)
      (goto-char (or (next-single-property-change (point) nav-prop)
                     (point)))
      (setq done (or (not (invisible-p (point)))
                     (eql prev (point))))
      (setq prev (point)))))


(defun egg-buffer-cmd-prev-block (nav-prop)
  "Move to the previous block indentified by text property NAV-PROP."
  (let ((done nil)
        (prev (point))) 
    (while (not done)
      (goto-char (previous-single-property-change (point) nav-prop
                                              nil (point-min)))
 
      (setq done (or (not (invisible-p (point)))
                     (eql prev (point))))
      (setq prev (point)))))



(defvar my-egg-restored-pt nil
  "Point restored by egg-restore-section-visibility")


(defadvice egg-restore-section-visibility (around remember-point-after-restoring-section-visibility activate)
  (setq ad-return-value ad-do-it)
  (setq my-egg-restored-pt (point)))

(defadvice egg-diff-section-cmd-unstage (before fix-point activate)
  (setq my-egg-restored-pt nil))

(defadvice egg-diff-section-cmd-stage (before fix-point activate)
  (setq my-egg-restored-pt nil))

(defadvice egg-diff-section-cmd-unstage (after fix-point activate)
  ;; (log-expr "here2 my-egg-restored-pt")
  (when my-egg-restored-pt
    (goto-char my-egg-restored-pt)))

(defadvice egg-diff-section-cmd-stage (after fix-point activate)
  ;; (log-expr "here1" my-egg-restored-pt)
  (when my-egg-restored-pt
    (goto-char my-egg-restored-pt)))

;; done in the end of my-emacs-setup anyway
;; (viper-apply-major-mode-modifiers)

(provide 'my-vc-setup)
