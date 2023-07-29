(require 'eglot)

(setq
 eglot-confirm-server-initiated-edits nil
 eglot-send-changes-idle-time 2
 ;; todo eglot-workspace-configuration '((:rust-analyzer . (:checkOnSave "clippy")))
 )

(define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)

(put 'eglot-note 'flymake-overlay-control nil)
(put 'eglot-warning 'flymake-overlay-control nil)
(put 'eglot-error 'flymake-overlay-control nil)

(defun eglot-customizations ()
  (eglot-inlay-hints-mode -1))

(add-hook 'eglot-managed-mode-hook 'eglot-customizations)
