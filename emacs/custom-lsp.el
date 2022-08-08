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
 )

;; setting lsp-headerline-breadcrumb-enable doesn't seem to make a difference
(defun lsp-customizations ()
  (lsp-headerline-breadcrumb-mode -1)
  ;; todo - how to make this less annoying
  ;; (setq-local auto-save-visited-interval 3)
  ;; (auto-save-visited-mode t)
  )

(add-hook 'lsp-mode-hook 'lsp-customizations)

(require 'yasnippet)
(add-hook 'lsp-mode-hook 'yas-minor-mode)

(add-hook 'rust-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'yas-minor-mode)

(define-key lsp-mode-map (kbd "<mouse-3>") nil)
