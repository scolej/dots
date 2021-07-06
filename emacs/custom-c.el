(defun c-setup ()
   ;; fixme does it even work?
  (setq-local parens-require-spaces nil)
  (setq-local c-basic-offset 4))

(add-hook 'c-mode-hook 'c-setup)
