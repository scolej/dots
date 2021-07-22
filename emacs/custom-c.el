(defun c-setup ()
   ;; fixme does it even work?
  (setq-local parens-require-spaces nil)
  (setq-local c-basic-offset 4)
  (electric-indent-mode 1))

(add-hook 'c-mode-hook 'c-setup)

(defun c-begin-block-comment ()
  (interactive)
  (save-excursion
    (insert "/*")
    (newline-and-indent)
    (delete-backward-char 1)
    (insert "*")
    (newline-and-indent)
    (insert "*/"))
  (forward-line)
  (end-of-line)
  (insert " "))
