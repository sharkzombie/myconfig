(add-to-list 'load-path "~/myconfig/emacs")

;;; standard emacs libraries
(require 'my-utils)
(require 'package)
(require 'saveplace)
(require 'paren)
(require 'compile)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(add-to-list 'load-path "~/myconfig/elisp")

;;; myconfig/elisp/site-lisp contains subdirs that will automatically go to load path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((my-lisp-dir "~/myconfig/elisp/site-lisp")
         (default-directory my-lisp-dir))
    (setq load-path (cons my-lisp-dir load-path))
    (normal-top-level-add-subdirs-to-load-path)))

(when (require-if-available 'color-theme)
  (load "my-color-theme")
  (my-color-theme))


;; ================ some basic settings =======================
(setq-default indent-tabs-mode nil)    ;; Tabs as spaces
(setq-default tab-always-indent t)


;; do I need below?  THey were in old my-emacs-setup.el

;; (setq-default help-char 0)      ;; Allow using Ctrl-H as normal key
;; (setq help-char 0)              ;; Allow using Ctrl-H as normal key
;; (global-set-key "\M-gh" help-map)
;; (setq help-event-list '(f1))
;; (setq interprogram-cut-function 'x-select-text)
;; (setq x-select-enable-clipboard t)

;; ;; ok someone keeps resetting this shit
;; (setq auto-hscroll-mode t)
;; (setq-default auto-hscroll-mode t)


;; =============================== utils, compile ====================

(require-if-available 'my-utils)
(require-if-available 'my-term-setup)

(when (or (require-if-available 'cua-base ) (require-if-available 'cua))
  (cua-mode 1))

(when (require-if-available 'undo-tree)
  (global-undo-tree-mode 1))
;; =================================== Evil =============================
(unless (package-installed-p 'evil)
  (package-install 'evil))

(unless (require-if-available 'evil)
  (error "Rest of this file requires evil, figure out why evil is not working"))

(defun my-after-major-mode-hook (&optional arg)
  "My `after-change-major-mode-hook' that runs mm/after-MODE-major-mode function if it exists"
  (let ((symbol (intern (format "mm/after-%s" major-mode))))
    (when (fboundp symbol)
      (ignore-errors
        (funcall symbol)
        (evil-normalize-keymaps)))))

(add-hook 'after-change-major-mode-hook 'my-after-major-mode-hook)

(require-if-available 'my-evil-setup)

;; =================== cycle-buffer and windows ========================

(require-if-available 'cycle-buffer 'my-cycle-buffer-setup)

;; why did I have below? Seems pretty old code
;; seems to be protection against quit-window called with NIL window

;; (defadvice quit-window (around mm/quit-window-kills-window activate)
;;   (if (and (null window)
;;            (not (one-window-p t)))
;;       (setq window (selected-window)))
;;   (setq ad-return-value ad-do-it))

;; ============================= Lisp editing ==========================

(when (require-if-available 'my-paredit-setup)
  (defun mm/after-emacs-lisp-mode ()
    (mm/magic-lisp-editing +1))
  (defun mm/after-lisp-interaction-mode ()
    (mm/magic-lisp-editing +1))
  (defun mm/after-lisp-mode ()
    (mm/magic-lisp-editing +1)))

;; ============================= various other modes setup =============
(require-if-available 'my-compile)
(require-if-available 'my-tempo-setup)
(require-if-available 'my-ccmode-setup)
(require-if-available 'my-shell-mode-setup)
(require-if-available 'my-cmake-setup)



;; Editing file without extension but with zsh in their name
;; is shell mode
(add-to-list 'auto-mode-alist (cons "zsh[^.]*$" 'shell-script-mode))
(add-to-list 'auto-mode-alist (cons "zalias$" 'shell-script-mode))
(add-to-list 'auto-mode-alist (cons "/X11.+app-override/" 'conf-xdefaults-mode))
(add-to-list 'auto-mode-alist (cons "\\.\\(ofx\\|qfx\\)$" 'nxml-mode))
(add-to-list 'auto-mode-alist (cons "\\.\\(h\\|hh\\)$" 'c++-mode))

(when (require-if-available 'jam-mode) 
  (add-to-list 'auto-mode-alist (cons "\\.\\(jam\\)$" 'jam-mode)))


