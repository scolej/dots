(define-key input-decode-map [?\C-i] [C-i])

(defvar experimove-mode-map
  (let ((map (make-sparse-keymap)))
    ;; movement
    (define-key map (kbd "C-j") 'backward-char)
    (define-key map (kbd "C-l") 'forward-char)
    (define-key map (kbd "<C-i>") 'previous-line)
    (define-key map (kbd "C-k") 'next-line)
    (define-key map (kbd "C-u") 'backward-word)
    (define-key map (kbd "C-o") 'forward-word)
    (define-key map (kbd "C-;") 'end-of-line)
    (define-key map (kbd "C-h") 'beginning-of-line-toggle)
    ;; deletion
    (define-key map (kbd "M-o") 'delete-forward-word)
    (define-key map (kbd "M-u") 'delete-backward-word)
    (define-key map (kbd "M-l") 'delete-forward-char)
    (define-key map (kbd "M-j") 'delete-backward-char)
    map))

(define-minor-mode experimove-mode
  ""
  :global t
  :keymap experimove-mode-map)
