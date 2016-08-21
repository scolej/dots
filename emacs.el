(setq debug-on-error nil)

(setq-default inhibit-startup-message t
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
              mode-line-format (list "(%Z %4l %3c) (%m) (%b) (%f) (%P)")
              revert-without-query '(".*")
              dired-listing-switches "-alh"
              truncate-partial-width-windows nil
              split-height-threshold 1200
              split-width-threshold 2000
              cursor-type 'box
              isearch-wrap-function '(lambda nil)
              large-file-warning-threshold 20000000
              c-basic-offset 4
              lazy-highlight-cleanup nil
              lazy-highlight-max-at-a-time nil
              show-trailing-whitespace nil
              scroll-conservatively 1000
              scroll-margin 20)

;; Make the cursor red in future frames. TODO :/ Doesn't work ??
;; Works if you don't call (set-face-attribute) for the cursor ??
(add-to-list 'default-frame-alist '(cursor-color . "red"))

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

(setf text-scale-mode-step 1.05)

;; Stop polluting the entire filesystem with backup files.
(if (boundp '*my-backup-dir*)
    (let ((dir *my-backup-dir*))
      (setq backup-directory-alist `((".*" . ,dir)))
      (setq auto-save-file-name-transforms `((".*" ,dir t)))))

(defun ttl ()
  (interactive)
  (toggle-truncate-lines))

(defun words-dammit ()
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

(add-hook 'text-mode-hook 'words-dammit)

;; Keys
(windmove-default-keybindings)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-c o") 'ffap)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'replace-string)
(global-set-key (kbd "<C-return>") 'set-rectangular-region-anchor)
(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<C-S-right>") 'forward-whitespace)
(global-set-key (kbd "<C-S-left>") (lambda () (interactive) (forward-whitespace -1)))
(global-set-key (kbd "C-c f c") 'make-frame)
(global-set-key (kbd "C-c f d") 'delete-frame)
(global-set-key (kbd "C-c f m") 'iconify-frame)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "M-s s") 'sort-lines)
(global-set-key (kbd "M-s d") 'delete-trailing-whitespace)
(global-set-key (kbd "<C-tab>") 'mode-line-other-buffer)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key [S-wheel-down] 'scroll-left)
(global-set-key [S-wheel-up] 'scroll-right)
(global-set-key [mouse-3] 'kill-region)
(global-set-key [mouse-2] 'mouse-yank-at-click)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(require 'use-package)

(prefer-coding-system 'utf-8)

(use-package cl
  :demand)

(use-package ivy
  :demand
  :config
  (setq-default ivy-re-builders-alist '((t . ivy--regex-ignore-order))
                ivy-use-virtual-buffers t
                ivy-height 15)
  (ivy-mode t)
  :bind (("C-b" . switch-to-buffer)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-S-s" . isearch-forward)))

(use-package dired
  :demand
  :bind
  (:map dired-mode-map ("<backspace>" . dired-up-directory)))

(use-package dired-x)

(use-package neotree
  :demand
  :config
  (setq-default neo-theme 'ascii
                neo-window-width 45)
  (add-hook 'neotree-mode-hook (lambda () (text-scale-set -5))))

(use-package dedicated)

(use-package ibuffer
  :demand
  :config
  (setq-default ibuffer-default-sorting-mode '(filename/process))
  :bind (("C-`" . ibuffer)
         ("<escape>" . ibuffer)))

(use-package shell
  :demand
  :config
  (setq-default comint-scroll-show-maximum-output nil))

(use-package drag-stuff
  :demand
  :pin melpa-stable
  :bind (("<M-down>" . drag-stuff-down)
         ("<M-up>" . drag-stuff-up)))

(use-package expand-region
  :demand
  :pin melpa-stable
  :bind (("M-u" . er/expand-region)))

(use-package hungry-delete
  :demand
  :bind (("<S-backspace>" . hungry-delete-backward)))

(use-package mwim
  :demand
  :pin melpa-stable
  :bind (("C-a" . mwim-beginning-of-code-or-line)))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-mode))

(use-package duplicate-thing
  :demand
  :bind (("M-d" . duplicate-thing)))

(use-package highlight-thing)

(use-package bm
  :demand
  :bind (("M-." . bm-next)
         ("M-," . bm-previous)
         ("<M-SPC>" . bm-toggle)))

(use-package solarized-theme
  :demand
  :config
  (setq-default solarized-use-less-bold t)
  (load-theme 'solarized-light)
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

(use-package emojify
  :demand
  :config
  (global-emojify-mode)
  (setq emojify-display-style 'image)) ;; 😊😍😡😲😳😷❤🚗🌛🌱🍃🍄

(use-package flycheck)

(use-package magit)

(use-package transpose-frame
  :bind ("C-x t" . transpose-frame))

(use-package git-gutter+)

(use-package visual-regexp)

(use-package rainbow-mode)

(use-package feature-mode)

(use-package haskell-mode)

(use-package markdown-mode)
