;; IDEAS
;;
;; * Change cursor colour to reflect save state.
;; * Prefix 1,2,3 bookmark for different colour bookmarks.
;; * Highlight mouse cursor pos always
;; * Mode line click yank buffer file path
;; * Long press right -> copy, short press right -> cut
;; * Flash text on copy?
;; * C-u split window operates on parent window. To easily add more cols / rows at top level.
;;
;; TODO
;;
;; * Has save predicate broken? Doesn't revert to unsaved if you add a space then delete a

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
              linum-format "%4d"
              isearch-allow-scroll t
              save-interprogram-paste-before-kill t
              ;; mode-line-format (list "(%Z %4l %3c) (%m) (%b) (%f) (%P)")
              revert-without-query '(".*")
              dired-listing-switches "-alh"
              truncate-partial-width-windows nil
              split-height-threshold 1200
              split-width-threshold 2000
              cursor-type 'box
              isearch-wrap-function '(lambda nil)
              large-file-warning-threshold 20000000
              c-basic-offset 4
              lazy-highlight-cleanup t
              lazy-highlight-max-at-a-time nil
              show-trailing-whitespace nil
              scroll-conservatively 9999
              scroll-margin 0
              recentf-max-saved-items 100)

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

;; Stop polluting the entire filesystem with backup files.
(if (boundp '*my-backup-dir*)
    (let ((dir *my-backup-dir*))
      (setq backup-directory-alist `((".*" . ,dir)))
      (setq auto-save-file-name-transforms `((".*" ,dir t)))))

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

;;
;; Hooks and such
;;

(add-hook 'text-mode-hook 'words-dammit)
(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)
(add-hook 'occur-hook 'occur-rename-buffer)

(add-to-list 'auto-mode-alist '("\\.log\\'" . read-only-mode))

;; Some goodness scrounged from http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/.
;; See if it helps ivy search do its thing or not.
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;
;; Longmouse
;;
;; Functions and bindings for long-pressing right mouse button for copy / cut.
;;

(defvar longmouse-timer nil)
(defun longmouse-down ()
  (interactive)
  (setq longmouse-timer
        (run-at-time 0.3
                     nil
                     '(lambda ()
                        (setq longmouse-timer nil)
                        (whole-line-or-region-delete 1)))))
(defun longmouse-up ()
  (interactive)
  (unless (eq longmouse-timer nil)
    (progn
      (whole-line-or-region-kill-ring-save 1)
      (setq deactivate-mark nil)
      (message "Saved region.")
      (cancel-timer longmouse-timer)
      (setq longmouse-timer nil))))
(global-set-key [down-mouse-3] 'longmouse-down)
(global-set-key [mouse-3] 'longmouse-up)
(global-set-key [mouse-2] 'whole-line-or-region-yank)

;;
;; Keys
;;

(windmove-default-keybindings)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-c o") 'ffap)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'replace-string)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "<C-return>") 'set-rectangular-region-anchor)
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
(global-set-key (kbd "<C-tab>") 'mode-line-other-buffer)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key [S-wheel-down] 'scroll-left)
(global-set-key [S-wheel-up] 'scroll-right)

;;
;; Packages
;;

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(require 'use-package)

(prefer-coding-system 'utf-8)

(use-package syntax-subword
  :demand
  :config
  (global-syntax-subword-mode t))

(use-package evil
  :config
  (setq-default evil-search-wrap nil)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex))

(use-package cl
  :demand)

(use-package ivy
  :demand
  :config
  (setq-default ivy-re-builders-alist '((t . ivy--regex-ignore-order))
                ivy-use-virtual-buffers t
                ivy-height 15)
  (ivy-mode t)
  :bind
  (("<escape>" . switch-to-buffer)))

(use-package swiper
  :demand
  :bind
  (("C-s" . swiper)
   ("C-S-s" . isearch-forward)))

(use-package back-button
  :demand
  :config
  (back-button-mode t)
  :bind
  (("<M-left>" . back-button-global-backward)
   ("<M-right>" . back-button-global-forward)))

(use-package projectile
  :config
  (projectile-global-mode t)
  (setq-default projectile-indexing-method 'alien
                projectile-completion-system 'ivy)
  :bind
  (("C-`" . projectile-find-file)
   ("C-~" . projectile-switch-project)))

;; (use-package dired
;;   :demand
;;   :bind
;;   (:map dired-mode-map ("<backspace>" . dired-up-directory)))

(use-package neotree
  :demand
  :config
  (setq-default neo-theme 'ascii
                neo-window-width 45))

(use-package ibuffer
  :demand
  :config
  (setq-default ibuffer-default-sorting-mode '(filename/process))
  :bind
  (("C-b" . ibuffer)))

(use-package expand-region
  :demand
  :pin melpa-stable
  :bind
  (("M-u" . er/expand-region)))

(use-package mwim
  :demand
  :pin melpa-stable
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-mode t))

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
  :bind
  (("M-." . bm-next)
   ("M-," . bm-previous)
   ("<M-SPC>" . bm-toggle)))

(use-package solarized-theme
  :demand
  :config
  (setq-default solarized-use-less-bold t)
  (load-theme 'solarized-dark)
  (set-face-attribute 'mode-line-inactive nil
                      :underline nil
                      :overline nil
                      :box '(:line-width 2 :style released-button))
  (set-face-attribute 'mode-line nil
                      :underline nil
                      :overline nil
                      :box '(:line-width 2 :style released-button))
  (set-face-attribute 'bm-face nil
                      :underline nil
                      :overline nil
                      :background "yellow"))

(use-package flycheck)

(use-package magit
  :config
  (setq-default auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p
                vc-handled-backends nil)
  :bind
  (("C-c m" . magit-status)))

(use-package transpose-frame
  :bind
  ("C-x t" . transpose-frame))

(use-package rainbow-mode)

(use-package feature-mode)

(use-package haskell-mode)

(use-package markdown-mode)

;; Make the cursor red in future frames. TODO :/ Doesn't work ??
;; Works if you don't call (set-face-attribute) for the cursor ??
(add-to-list 'default-frame-alist '(cursor-color . "red"))
