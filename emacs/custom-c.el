(defun c-setup ()
  (setq-local
   ;; fixme does it even work?
   parens-require-spaces nil
   c-basic-offset 4))

(add-hook c-mode-hook 'c-setup)
