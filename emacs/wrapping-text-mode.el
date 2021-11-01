(define-derived-mode wrapping-text-mode fundamental-mode
  (toggle-truncate-lines -1)
  (visual-line-mode t))

(add-to-list 'auto-mode-alist '("\\.tx\\'" . wrapping-text-mode))

(provide 'wrapping-text-mode)
