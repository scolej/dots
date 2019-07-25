(define-minor-mode clean-trailing-whitespace-mode
  "Minor mode to automatically clean trailing whitespace on save."
  nil " tw" nil
  (if clean-trailing-whitespace-mode
      (progn
        (add-hook 'before-save-hook 'delete-trailing-whitespace)
        (setq show-trailing-whitespace nil))
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)
    ;; (kill-local-variable show-trailing-whitespace)
    (setq show-trailing-whitespace t)))

(defun current-buffer-has-trailing-whitespace-p ()
  (beginning-of-buffer)
  (re-search-forward "[[:space:]]+$" nil t))

(defun maybe-tidy-whitespace ()
  (unless (current-buffer-has-trailing-whitespace-p)
    (clean-trailing-whitespace-mode 1)))

(add-hook 'find-file-hooks 'maybe-tidy-whitespace)