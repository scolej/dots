(setq-default c-basic-offset 4
              cursor-type 'box
              dired-listing-switches "-alh"
              dired-auto-revert-buffer t
              hi-lock-auto-select-face t
              indent-tabs-mode nil
              inhibit-startup-message t
              initial-scratch-message nil
              isearch-allow-scroll t
              isearch-wrap-function '(lambda nil)
              large-file-warning-threshold nil
              lazy-highlight-cleanup t
              lazy-highlight-max-at-a-time nil
              linum-format "%4d"
              mode-line-format "%b %*"
              mouse-autoselect-window 0.5
              mouse-wheel-progressive-speed nil
              mouse-wheel-scroll-amount '(4 ((shift) . 4))
              recentf-max-saved-items 100
              revert-without-query '(".*")
              ring-bell-function 'ignore
              save-interprogram-paste-before-kill t
              sentence-end-double-space nil
              set-mark-command-repeat-pop t
              scroll-conservatively 9999
              scroll-margin 0
              show-help-function nil
              show-paren-style 'parenthesis
              show-trailing-whitespace nil
              tab-width 4
              truncate-lines t
              truncate-partial-width-windows nil
              visible-bell nil)

;; Backup set up.
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode t)
(transient-mark-mode t)
(recentf-mode t)
(savehist-mode t)
(delete-selection-mode t)
(column-number-mode t)
(global-hl-line-mode 0)
(cua-mode 0)

(defun ttl ()
  "Shortcut for truncating lines."
  (interactive)
  (toggle-truncate-lines))

(defun words-dammit ()
  "I just want word wrapping!"
  (interactive)
  (toggle-truncate-lines 0)
  (visual-line-mode t))

(defun copy-buffer-path ()
  "Copy the full path to the current buffer's file."
  (interactive)
  (kill-new (buffer-file-name)))

(add-hook 'occur-hook 'occur-rename-buffer)

(global-set-key (kbd "C-c f c") 'make-frame)
(global-set-key (kbd "C-c f d") 'delete-frame)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-s s") 'sort-lines)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key [S-wheel-down] '(lambda () (interactive) (scroll-left 20)))
(global-set-key [S-wheel-up] '(lambda () (interactive) (scroll-right 20)))
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-z") 'undo)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "M-S-y") 'yank-pop-forwards)

(use-package dired
  :bind (:map dired-mode-map
              ("<backspace>" . dired-up-directory)
              ("C-t" . nil)))

(use-package dired-x
  :bind (("M-j" . dired-jump)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-line-or-code)))

(use-package transpose-frame
  :bind (("C-x t" . transpose-frame)))

(use-package auto-complete
  :demand
  :config
  (ac-config-default)
  (setq ac-auto-show-menu t
        ac-use-quick-help nil)
  :bind (:map ac-completing-map
              ("<down>" . nil)
              ("<up>" . nil)
              ("<RET>" . nil)
              ("<return>" . nil)
              ("<backtab>" . ac-previous)
              ("C-p" . ac-previous)
              ("C-n" . ac-next)
              ("C-j" . ac-complete)
              ("<escape>" . keyboard-quit)))

(use-package multiple-cursors
  :bind (("C-S-n" . mc/mark-next-like-this)
         ("<M-S-down>" . mc/mark-next-lines)
         ("<M-S-up>" . mc/mark-previous-lines)))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package haskell-mode)

(use-package hindent
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package bm
  :bind (("<M-SPC>" . bm-toggle)
         ("<M-up>" . bm-previous)
         ("<M-down>" . bm-next)))

(use-package markdown-mode)

(use-package ivy
  :config
  (ivy-mode)
  (setq-default ivy-use-virtual-buffers t)
  :bind (("<escape>" . ivy-switch-buffer)
         :map ivy-switch-buffer-map
         ("<escape>" . minibuffer-keyboard-quit)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-S-s" . isearch-forward)))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel
  :bind (("C-c j" . counsel-git-grep)))

(load "save-all-the-things.el")

(use-package rust-mode)

(use-package flycheck-rust
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package drag-stuff
  :bind (("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

(use-package duplicate-thing
  :init
  (defun duplicate-thing-down ()
    (interactive)
    (duplicate-thing 1)
    (next-line))
  :bind (("<C-M-up>" . duplicate-thing)
         ("<C-M-down>" . duplicate-thing-down)))
