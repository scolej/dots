(setq js-indent-level 4)

;; (add-hook 'js-mode-hook 'prettier-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun format-prettier ()
  (interactive)
  (save-buffer)
  (call-process "prettier" nil "*prettier*" nil
                "--write" (buffer-file-name)))

(define-key js-mode-map (kbd "C-c C-f") 'format-prettier)
