(setq custom-file "~/.emacs.d/ignore-me.el")

(setq-default cursor-type '(bar . 4)
              cursor-in-non-selected-windows nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(eldoc-mode -1)
(tooltip-mode -1)

(scroll-bar-mode -1)

(delete-selection-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(show-paren-mode 1)

(setq c-basic-offset 4

      save-interprogram-paste-before-kill t

      backup-by-copying t
      backup-directory-alist '((".*" . "~/.emacs.d/saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 6
      version-control t
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t))
      auto-save-timeout 5
      auto-save-interval 100
      create-lockfiles nil

      inhibit-startup-screen t
      initial-scratch-message nil

      dired-auto-revert-buffer t
      dired-clean-confirm-killing-deleted-buffers nil
      dired-use-ls-dired nil

      require-final-newline nil
      mode-require-final-newline nil

      sentence-end-double-space nil
      set-mark-command-repeat-pop t
      ring-bell-function 'ignore
      linum-format "%4d"
      mouse-autoselect-window -0.1
      revert-without-query '(".*")
      mouse-wheel-progressive-speed nil
      read-buffer-completion-ignore-case t

      vc-handled-backends nil

      scroll-conservatively 0)

(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'savehist)
(savehist-mode 1)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<M-delete>") 'kill-word)
