(setq custom-file "~/.emacs.d/custom.el")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-auto-revert-mode 1)
(recentf-mode 1)
(show-paren-mode 1)

(setq indent-tabs-mode nil
      c-basic-offset 4

      save-interprogram-paste-before-kill t

      backup-by-copying t
      backup-directory-alist '((".*" . "~/.emacs.d/saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 6
      version-control t
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t))
      auto-save-timeout 20
      create-lockfiles nil

      inhibit-startup-screen t
      initial-scratch-message nil

      dired-auto-revert-buffer t
      dired-clean-confirm-killing-deleted-buffers nil
      dired-use-ls-dired nil

      sentence-end-double-space nil
      set-mark-command-repeat-pop t
      ring-bell-function 'ignore
      linum-format "%4d"
      mouse-autoselect-window -0.1
      revert-without-query '(".*"))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'text-mode-hook 'visual-line-mode)

(require 'dired-x)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(require 'savehist)
(savehist-mode 1)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
