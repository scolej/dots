(define-derived-mode messy-log-view-mode fundamental-mode "messy log view mode"
  (strip-ansi-current-buffer)
  (delete-trailing-whitespace)
  (view-mode 1))

(add-to-list 'auto-mode-alist '("messy\\.log\\'" . messy-log-view-mode))
