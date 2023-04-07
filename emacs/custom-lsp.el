(require 'lsp)

(setq
 lsp-completion-provider :none
 lsp-completion-show-detail nil
 lsp-completion-show-kind nil
 lsp-diagnostics-provider :flymake
 lsp-headerline-breadcrumb-enable nil
 lsp-idle-delay 1
 lsp-lens-enable nil
 lsp-modeline-code-actions-enable nil
 lsp-signature-auto-activate nil
 lsp-ui-sideline-enable nil
 lsp-auto-execute-action nil
 )

;; setting lsp-headerline-breadcrumb-enable doesn't seem to make a difference
(defun lsp-customizations ()
  (lsp-headerline-breadcrumb-mode -1))

(add-hook 'lsp-mode-hook 'lsp-customizations)

(require 'yasnippet)
(add-hook 'lsp-mode-hook 'yas-minor-mode)

;; (add-hook 'rust-mode-hook 'lsp)
;; (add-hook 'rust-mode-hook 'yas-minor-mode)

(define-key lsp-mode-map (kbd "<mouse-3>") nil)
(define-key lsp-mode-map (kbd "s-l r") 'lsp-rename)
(define-key lsp-mode-map (kbd "s-l a") 'lsp-execute-code-action)


;; todo, would like: when i find a file, if it's under a workspace dir,
;; then connect, otherwise do nothing; don't ask
