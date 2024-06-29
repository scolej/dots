(require 'eglot)

(add-to-list 'eglot-server-programs '(rust-mode "/opt/homebrew/Cellar/rust-analyzer/2024-05-13/bin/rust-analyzer"))

(setq
 eglot-confirm-server-initiated-edits nil
 eglot-send-changes-idle-time 2
 eglot-connect-timeout nil
 ;; todo eglot-workspace-configuration '((:rust-analyzer . (:checkOnSave "clippy")))
 )

(define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-/") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-.") 'flymake-goto-next-error)
(define-key eglot-mode-map (kbd "C-,") 'flymake-goto-prev-error)

(put 'eglot-note 'flymake-overlay-control nil)
(put 'eglot-warning 'flymake-overlay-control nil)
(put 'eglot-error 'flymake-overlay-control nil)

(defun eglot-customizations ()
  (eglot-inlay-hints-mode -1)
  (eldoc-mode -1))

(add-hook 'eglot-managed-mode-hook 'eglot-customizations)
