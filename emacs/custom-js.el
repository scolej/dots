(add-to-list
 'auto-mode-alist
 '("\\.tsx\\'" . typescript-mode))

(add-hook
 'js-mode-hook
 'prettier-mode)

(setq js-indent-level 2)
