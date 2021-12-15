(setq-default ruby-indent-level 4)

(require 'ruby-mode)
(add-hook 'ruby-mode-hook 'yas-minor-mode)

;; (define-key ruby-mode-map (kbd "<tab>") 'yas-expand)
