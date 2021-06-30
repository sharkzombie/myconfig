(require 'edebug)

(evil-define-key "n" '(motion-state) )

(evil-define-key '(motion) 'edebug-mode-map "n" 'edebug-next-mode)
(evil-define-key '(motion) 'edebug-mode-map "s" 'edebug-step-mode)
(evil-define-key '(motion) 'edebug-mode-map "c" 'edebug-continue-mode)
