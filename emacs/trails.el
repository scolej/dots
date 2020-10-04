;;; Tidy whitespace.
;;;
;;; If buffer has no trailing whitespace, keep it that way. Otherwise
;;; show the trailing whitespace.

(define-minor-mode clean-trailing-whitespace-mode
  "Minor mode to automatically clean trailing whitespace on save."
  nil " tw" nil
  (if clean-trailing-whitespace-mode
      (progn
        ;; Enable mode
        (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
        (setq show-trailing-whitespace nil))
    (progn
      ;; Disable mode
      (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
      ;; Revert to configured default
      (kill-local-variable 'show-trailing-whitespace))))

(defun current-buffer-has-trailing-whitespace-p ()
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "[\t ]+$" nil t)))

(defun maybe-tidy-whitespace-mode ()
  (if (current-buffer-has-trailing-whitespace-p)
      (setq show-trailing-whitespace t)
    (clean-trailing-whitespace-mode 1)))

(add-hook 'find-file-hook 'maybe-tidy-whitespace-mode)
(add-hook 'after-save-hook 'maybe-tidy-whitespace-mode)
