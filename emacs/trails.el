;; TODO Potentially enable on save.

(define-minor-mode clean-trailing-whitespace-mode
  "Minor mode to automatically clean trailing whitespace on save."
  nil " tw" nil
  (if clean-trailing-whitespace-mode
      (progn
        (add-hook 'before-save-hook 'delete-trailing-whitespace)
        (setq show-trailing-whitespace nil))
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)))

(defun current-buffer-has-trailing-whitespace-p ()
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "[[:space:]]+$" nil t)))

(defun maybe-tidy-whitespace ()
  (if (current-buffer-has-trailing-whitespace-p)
      (setq show-trailing-whitespace t)
    (clean-trailing-whitespace-mode 1)))

(add-hook 'find-file-hook 'maybe-tidy-whitespace)
