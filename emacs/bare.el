
(setq custom-file "~/.emacs.d/custom.el")

(tool-bar-mode -1)
(menu-bar-mode -1)
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
      auto-save-timeout 20
      create-lockfiles nil

      inhibit-startup-screen t
      initial-scratch-message nil

      dired-auto-revert-buffer t
      dired-clean-confirm-killing-deleted-buffers nil
      dired-use-ls-dired nil

      require-final-newline nil
      mode-require-final-newline nil

      ring-bell-function nil
      show-trailing-whitespace t
      sentence-end-double-space nil
      set-mark-command-repeat-pop t
      ring-bell-function 'ignore
      linum-format "%4d"
      mouse-autoselect-window -0.1
      revert-without-query '(".*")
      mouse-wheel-progressive-speed nil
      read-buffer-completion-ignore-case t

      vc-handled-backends nil

      split-width-threshold nil)

(setq-default indent-tabs-mode nil
              truncate-lines t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Also does derived modes :( (add-hook 'text-mode-hook 'visual-line-mode)

(require 'dired-x)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file)
(global-set-key (kbd "<escape>") 'dired-jump)

(require 'savehist)
(savehist-mode 1)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-/") 'hippie-expand)

(defun maybe-copy-whole-line ()
  (interactive)
  (if mark-active
      (copy-region-as-kill nil nil t)
    (beginning-of-line 2)
    (if (eq last-command 'maybe-copy-whole-line)
        (kill-append (buffer-substring-no-properties (point) (line-beginning-position 0))
                     nil)
    (kill-ring-save (point) (line-beginning-position 0)))))
(global-set-key (kbd "M-w") 'maybe-copy-whole-line)

(winner-mode 1)
(global-set-key (kbd "<C-wheel-down>") 'winner-undo)
(global-set-key (kbd "<C-wheel-up>") 'winner-redo)
