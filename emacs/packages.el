(when (require 'shellbow nil t)
  (add-to-list 'auto-mode-alist '("shellbow" . shellbow-mode))
  (global-set-key (kbd "<f11>") (lambda () (interactive)
                                  (save-some-buffers t)
                                  (shellbow-speedy-rerun))))

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
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'scheme-mode-hook 'lispy-mode)
  (lispy-set-key-theme '(special lispy))
  (dolist (m (list lispy-mode-map-parinfer
                   lispy-mode-map-lispy))
    (define-key m (kbd "C-a") nil)
    (define-key m (kbd "M-m") nil)
    (define-key m (kbd "<C-return>") nil)
    (define-key m (kbd ":") nil)
    (define-key m (kbd "M-o") nil)))

(when (require 'selected nil t)
  (defun query-replace-with-region (replacement)
    (interactive "sReplacement: ")
    (let ((str (buffer-substring-no-properties (point) (mark))))
      (deactivate-mark)
      (goto-char (min (point) (mark)))
      (query-replace-regexp str replacement)))

  (define-key selected-keymap (kbd "<return>") 'kill-ring-save)
  (define-key selected-keymap (kbd "/") 'replace-string)
  (define-key selected-keymap (kbd "r") 'query-replace-with-region)
  (global-set-key (kbd "<C-return>") 'yank)
  (selected-global-mode))

(when (require 'flycheck nil t)
  (setq flycheck-idle-change-delay 2))
