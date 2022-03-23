(setq js-indent-level 2)

(add-hook 'js-mode-hook 'prettier-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
