(when (package-installed-p 'projectile)
  (require 'projectile)
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (global-set-key (kbd "<f2>") 'projectile-find-file)
  (setq projectile-indexing-method 'alien
        projectile-completion-system 'default
        projectile-switch-project-action 'projectile-dired))

(when (package-installed-p 'ag)
  (require 'ag)
  (define-key ag-mode-map (kbd "n") 'compilation-next-and-visit)
  (define-key ag-mode-map (kbd "p") 'compilation-prev-and-visit)

  (defun pop-and-sole (buf)
    (pop-to-buffer buf)
    (delete-other-windows))

  (defun ag-here (str)
    (interactive "MAg literal: ")
    (pop-and-sole (ag str default-directory)))
  (define-key dired-mode-map (kbd "r") 'ag-here)
  (define-key ag-mode-map (kbd "r") 'ag-here)

  (defun ag-here-regexp (str)
    (interactive "MAg regexp: ")
    (pop-and-sole (ag-regexp str default-directory)))
  (define-key dired-mode-map (kbd "M-r") 'ag-here-regexp)
  (define-key ag-mode-map (kbd "M-r") 'ag-here-regexp)

  (add-hook 'ag-mode-hook (lambda () (setq truncate-lines t))))

(when (package-installed-p 'lispy)
  (require 'lispy)
  (lispy-set-key-theme '(special lispy))
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'scheme-mode-hook 'lispy-mode))

(when (package-installed-p 'ivy)
  (require 'ivy)
  (ivy-mode)
  (add-to-list 'ivy-completing-read-handlers-alist '(org-capture-refile . completing-read-default))
  (add-to-list 'ivy-completing-read-handlers-alist '(org-refile . completing-read-default)))