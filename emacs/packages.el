(when (package-installed-p 'projectile)
  (require 'projectile)
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (global-set-key (kbd "<f2>") 'projectile-find-file)
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'default))

(when (package-installed-p 'ag)
  (require 'ag)
  (defun ag-here (str) (interactive "MAg literal: ") (ag str default-directory))
  (define-key dired-mode-map (kbd "r") 'ag-here)
  (define-key ag-mode-map (kbd "r") 'ag-here))

(when (package-installed-p 'lispy)
  (require 'lispy)
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))