(require 'js)

(setq js-indent-level 4)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun ts-customizations ()
  (setq-local inhibit-clean-trailing-whitespace-mode t))
(add-hook 'typescript-mode-hook 'ts-customizations)
