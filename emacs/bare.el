(setq custom-file "~/.emacs.d/custom.el")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-font-lock-mode -1)

(global-auto-revert-mode 1)
(recentf-mode 1)

(setq-default tab-width 4
              indent-tabs-mode nil
              c-basic-offset 4
              sentence-end-double-space nil
              make-backup-files nil
              create-lockfiles nil
              auto-save-default nil
              inhibit-startup-screen t
              initial-scratch-message nil)

(add-hook 'text-mode-hook 'visual-line-mode)

(require 'dired-x)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(require 'savehist)
(savehist-mode 1)
