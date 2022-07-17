(require 'eglot)

(setq
 eglot-confirm-server-initiated-edits nil)

(define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
