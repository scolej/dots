(require 'eglot)

(setq
 eglot-confirm-server-initiated-edits nil
 eglot-send-changes-idle-time 2
 ;; todo eglot-workspace-configuration '((:rust-analyzer . (:checkOnSave "clippy")))
 )

(define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)

;;

;; todo how to disable the annoying buttons?

;; (defun eglot-setup ()
;;   (setq-local mouse-1-click-follows-link nil))

;; (add-hook eglot-managed-mode-hook 'eglot-setup)


;; (put 'eglot-node 'flymake-overlay-control nil)
;; (put 'eglot-warning 'flymake-overlay-control nil)
;; (put 'eglot-error 'flymake-overlay-control nil)
