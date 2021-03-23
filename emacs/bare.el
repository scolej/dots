;;
;; Minimal configuration.
;;

(setq custom-file "~/.emacs.d/ignore-me.el")

(setq-default cursor-type 'box
              cursor-in-non-selected-windows 'hollow)

(require 'savehist)

(blink-cursor-mode -1)
(eldoc-mode -1)
(fringe-mode nil)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(recentf-mode -1)

(savehist-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
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
      dired-recursive-deletes 'top      ; always?

      require-final-newline t
      mode-require-final-newline t

      sentence-end-double-space nil
      set-mark-command-repeat-pop nil
      ring-bell-function 'ignore
      linum-format "%4d"
      mouse-autoselect-window -0.1
      revert-without-query '(".*")
      mouse-wheel-progressive-speed nil
      vc-handled-backends '(Git)
      case-fold-search t

      read-buffer-completion-ignore-case t
      completion-ignore-case t

      ;; Things which this affects:
      ;; - 'up' in Info mode
      scroll-conservatively 0
      scroll-margin 0
      scroll-step 0

      use-dialog-box nil
      async-shell-command-buffer 'new-buffer
      split-width-threshold nil)

(setq-default indent-tabs-mode nil
              truncate-lines t)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)

(setenv "PAGER" "cat")
