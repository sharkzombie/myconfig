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

;; =================================== Evil, CUA, cycle-buffer, variou =============================
(unless (package-installed-p 'evil)
  (package-install 'evil))

(unless (require-if-available 'evil)
  (error "Rest of this file requires evil, figure out why evil is not working"))

(require-if-available 'my-evil-setup)
(require-if-available 'cycle-buffer 'my-cycle-buffer-setup)
(require-if-available 'my-shell-mode-setup)
(require-if-available 'my-compile)
(require-if-available 'my-tempo-setup)
(require-if-available 'my-ccmode-setup)
(require-if-available 'my-cmake-setup)

;; =================================== Cycle buffer =====================


;; Editing file without extension but with zsh in their name
;; is shell mode
(add-to-list 'auto-mode-alist (cons "zsh[^.]*$" 'shell-script-mode))
(add-to-list 'auto-mode-alist (cons "zalias$" 'shell-script-mode))
(add-to-list 'auto-mode-alist (cons "/X11.+app-override/" 'conf-xdefaults-mode))
(add-to-list 'auto-mode-alist (cons "\\.\\(ofx\\|qfx\\)$" 'nxml-mode))
(add-to-list 'auto-mode-alist (cons "\\.\\(h\\|hh\\)$" 'c++-mode))

(when (require-if-available 'jam-mode) 
  (add-to-list 'auto-mode-alist (cons "\\.\\(jam\\)$" 'jam-mode)))


