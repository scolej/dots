(setq debug-on-error nil)

(setq-default inhibit-startup-message t
              initial-scratch-message nil
              visible-bell nil
              ring-bell-function 'ignore
              mouse-wheel-scroll-amount '(4 ((shift) . 4))
              mouse-wheel-progressive-speed nil
              show-paren-style 'expression
              truncate-lines t
              show-help-function nil
              hi-lock-auto-select-face t
              indent-tabs-mode nil
              tab-width 4
              linum-format "%4d"
              isearch-allow-scroll t
              save-interprogram-paste-before-kill t
              revert-without-query '(".*")
              dired-listing-switches "-alh"
              truncate-partial-width-windows nil
              split-height-threshold 1200
              split-width-threshold 2000
              cursor-type 'box
              isearch-wrap-function '(lambda nil)
              large-file-warning-threshold nil
              c-basic-offset 4
              lazy-highlight-cleanup t
              lazy-highlight-max-at-a-time nil
              show-trailing-whitespace nil
              scroll-conservatively 9999
              scroll-margin 0
              recentf-max-saved-items 100)

(prefer-coding-system 'utf-8)

;;
;; Enable built in modes
;;

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
(global-hl-line-mode t)
(winner-mode t)

;;
;; Little shortcuts
;;

(defun ttl ()
  "Shortcut for truncating lines."
  (interactive)
  (toggle-truncate-lines))

(defun toggle-pin ()
  "Toggle pinning on a window to keep it around."
  (interactive)
  (if (window-dedicated-p)
      (set-window-dedicated-p nil nil)
    (set-window-dedicated-p nil t)))
(global-set-key (kbd "C-x p") 'toggle-pin)

(defun words-dammit ()
  "I just want word wrapping!"
  (interactive)
  (toggle-truncate-lines 0)
  (visual-line-mode t))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))

(defun copy-buffer-path ()
  (interactive)
  (kill-new (buffer-file-name)))

;;
;; Hooks and such
;;

(add-hook 'text-mode-hook 'words-dammit)
;; (add-hook 'isearch-mode-end-hook 'recenter-top-bottom)
(add-hook 'occur-hook 'occur-rename-buffer)

(add-hook 'find-file-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "log")
              (read-only-mode t)
              (auto-revert-mode t))))

;; (defadvice compile-goto-error (after recenter-after-selecting)
;;   (interactive)
;;   (recenter)
;;   (message "hello"))

;;
;; Keys
;;

(windmove-default-keybindings)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-c o") 'ffap)
(global-set-key (kbd "C-c r") 'revert-buffer)
;; (global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'replace-string)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<C-S-right>") 'forward-whitespace)
(global-set-key (kbd "<C-S-left>") (lambda () (interactive) (forward-whitespace -1)))
(global-set-key (kbd "C-c f c") 'make-frame)
(global-set-key (kbd "C-c f d") 'delete-frame)
(global-set-key (kbd "C-c f m") 'iconify-frame)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-r") 'speedy-grep-rgrep)
(global-set-key (kbd "M-s s") 'sort-lines)
(global-set-key (kbd "M-s d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<C-tab>") 'mode-line-other-buffer)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key [S-wheel-down] '(lambda () (interactive) (scroll-left 5)))
(global-set-key [S-wheel-up] '(lambda () (interactive) (scroll-right 5)))

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "M-S-y") 'yank-pop-forwards)

;; TODO
;; Remove C-d from groovy-mode-map
;; Remove <tab> from feature mode map

;;
;; Packages
;;

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(require 'use-package)

;; Built in

(use-package dired
  :ensure nil
  :demand
  :bind
  (:map
   dired-mode-map
   ("<backspace>" . dired-up-directory)
   ("<up-mouse-1>" . dired-mouse-find-file)))

(use-package ibuffer
  :ensure nil
  :demand
  :config
  (setq-default ibuffer-default-sorting-mode '(filename/process))
  (defun ibuffer-really-unmark-all ()
    "Unmark all the things in ibuffer."
    (interactive)
    (ibuffer-unmark-all ?\*))
  :bind
  (("C-x b" . ibuffer)
   :map ibuffer-mode-map
   ("U" . ibuffer-really-unmark-all)))

(use-package speedbar
  :ensure nil
  :config
  (speedbar-add-supported-extension ".hs")
  (setq-default speedbar-show-unknown-files t
                speedbar-use-images nil))

(use-package nxml-mode
  :ensure nil
  :config
  (add-hook 'nxml-mode-hook
            (lambda () (toggle-truncate-lines nil))))

;; Extra

(use-package multiple-cursors
  :disabled t
  :config
  (setq-default mc/always-run-for-all t)
  :bind
  (("<C-return>" . set-rectangular-region-anchor)
   ("C-c n" . mc/mark-next-like-this)))

(use-package syntax-subword
  :demand
  :config
  (global-syntax-subword-mode t))

(use-package cl
  :demand)

(use-package ivy
  :demand
  :config
  (setq-default ivy-re-builders-alist '((t . ivy--regex))
                ivy-use-virtual-buffers t
                ivy-height 15)
  (ivy-mode t)
  :bind
  (( ;; "<escape>" . switch-to-buffer)
   :map ivy-minibuffer-map
   ("<tab>" . ivy-partial)
   ("<escape>" . minibuffer-keyboard-quit)
   :map ivy-switch-buffer-map
   ("<escape>" . minibuffer-keyboard-quit)))

(use-package swiper
  :demand
  :config
  (defun smart-swiper ()
    (interactive)
    (let ((init-input (if (region-active-p)
                          (buffer-substring (mark) (point))
                        "")))
      (deactivate-mark)
      (swiper init-input)))
  :bind
  (("C-s" . smart-swiper)
   ("C-S-s" . isearch-forward)))

(use-package back-button
  :demand
  :config
  (back-button-mode t)
  :bind
  (("<M-left>" . back-button-local-backward)
   ("<M-right>" . back-button-local-forward)))

(use-package projectile
  :config
  (projectile-global-mode t)
  (setq-default projectile-indexing-method 'alien
                projectile-completion-system 'ivy)
  :bind
  (("C-`" . projectile-find-file)
   ("C-~" . projectile-switch-project)))

(use-package dired-x
  :demand
  :config
  (setq-default dired-omit-files-p t
                dired-auto-revert-buffer t))

(use-package expand-region
  :demand
  :pin melpa-stable
  :bind
  (("M-u" . er/expand-region)))

(use-package mwim
  :demand
  :pin melpa-stable
  :bind
  (("C-a" . mwim-beginning-of-line-or-code)))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-mode t)
  :bind
  (("C-v" . whole-line-or-region-yank)))

(use-package duplicate-thing
  :demand
  :config
  (defun duplicate-down () (interactive) (duplicate-thing 1) (next-line))
  :bind
  (("<C-M-down>" . duplicate-down)
   ("<C-M-up>" . duplicate-thing)))

(use-package highlight-thing)

(use-package bm
  :demand
  :config
  (setq-default bm-recenter t)
  :bind
  (("M-." . bm-next)
   ("M-," . bm-previous)
   ("<M-SPC>" . bm-toggle)))

(use-package lenlen-theme)

(use-package flycheck)

(use-package magit
  :demand
  :config
  (setq-default auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p
                vc-handled-backends nil)
  (add-hook 'magit-status-mode-hook 'delete-other-windows)
  :bind
  (("C-c m" . magit-status)))

(use-package transpose-frame
  :bind
  ("C-x t" . transpose-frame))

(use-package rainbow-mode)

(use-package feature-mode)

(use-package haskell-mode
  :config
  (setq-default flycheck-ghc-search-path '("src" "test"))
  (add-hook 'haskell-mode-hook 'hindent-mode)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;; (setq haskell-process-type 'cabal-repl)
  )

(use-package markdown-mode)

(use-package drag-stuff
  :config
  (drag-stuff-global-mode t)
  ;; (drag-stuff-define-keys)
  (unless (null drag-stuff-mode) (add-hook 'git-rebase-mode-hook '(lambda () (drag-stuff-mode -1))))
  :bind
  (("<M-up>" . drag-stuff-up)
   ("<M-down>" . drag-stuff-down)))

(use-package evil
  :config
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-prefix-command 'leader-map)
  (define-key evil-normal-state-map (kbd "\\") 'leader-map)
  (define-key 'leader-map (kbd "b") 'ivy-switch-buffer)
  (define-key 'leader-map (kbd "j") 'dired-jump)
  (define-key 'leader-map (kbd "m") 'magit-status)
  (define-key 'leader-map (kbd "w") 'delete-trailing-whitespace)
  (evil-mode t))

;; Make the cursor red.
(add-to-list 'default-frame-alist '(cursor-color . "red"))
(set-cursor-color "red")

;; Make the modeline small.
(set-face-attribute 'mode-line-inactive nil :height 0.7)
(set-face-attribute 'mode-line  nil :height 0.7)
