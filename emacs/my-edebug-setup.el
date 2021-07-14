(require 'edebug)

;; this currently does not work, need "evil-give-back-keys" from old config
(evil-define-key '(normal motion) edebug-mode-map "n" 'edebug-next-mode)
(evil-define-key '(normal) edebug-mode-map "s" 'edebug-step-mode)
(evil-define-key '(normal motion) edebug-mode-map "c" 'edebug-continue-mode)

(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

(provide 'my-edebug-setup)

