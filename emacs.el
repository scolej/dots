(setq debug-on-error nil)

(setq-default inhibit-startup-message t
              visible-bell nil
              ring-bell-function 'ignore
              mouse-wheel-scroll-amount '(4 ((shift) . 4))
              mouse-wheel-progressive-speed nil
              mouse-drag-copy-region t
              show-paren-style 'expression
              truncate-lines t
              show-help-function nil
              hi-lock-auto-select-face t
              indent-tabs-mode nil
              linum-format "%4d"
              isearch-allow-scroll t
              save-interprogram-paste-before-kill t
              mode-line-format (list "%Z %6l %2c > %m; %b; %f; %P")
              revert-without-query '(".*")
              dired-listing-switches "-alh"
              truncate-partial-width-windows nil
              split-height-threshold 1200
              split-width-threshold 2000
              cursor-type 'box
              isearch-wrap-function '(lambda nil))

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

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
;; (global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'replace-string)
(global-set-key (kbd "<C-return>") 'set-rectangular-region-anchor)
(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "C-c f c") 'make-frame)
(global-set-key (kbd "C-c f d") 'delete-frame)
(global-set-key (kbd "M-s s") 'sort-lines)
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

;; (setq use-package-always-ensure t)

(use-package cl
  :demand)

;; (use-package direx
;;   :demand
;;   :bind (("<S-escape>" . direx:jump-to-directory)
;;          :map direx:direx-mode-map
;;          ;; ("<escape>" . quit-window)
;;          ))

(use-package neotree
  :demand
  :config
  (setq-default neo-theme 'ascii)
  (add-hook 'neotree-mode-hook (lambda () (text-scale-set -5))))

(use-package dedicated)

(use-package ibuffer
  :demand
  :config
  (setq-default ibuffer-default-sorting-mode '(filename/process))
  ;; (add-to-list 'ibuffer-mode-hook
  ;;              (lambda ()
  ;;                (ibuffer-auto-mode t)
  ;;                (hl-line-mode t)))
  :bind (("C-`" . ibuffer)
         ("<escape>" . ibuffer)
         ;; :map ibuffer-mode-map
         ;; ("<escape>" . ibuffer-quit)
         ))

(use-package shell
  :demand
  :config
  (setq-default comint-scroll-show-maximum-output nil))

;; (use-package company
;;   :demand
;;   :config
;;   (global-company-mode t)
;;   (setq-default company-minimum-prefix-length 2)
;;   :bind (("<M-tab>" . company-complete)
;;          :map shell-mode-map
;;          ("<tab>" . company-complete)))

;; (use-package ivy
;;   :demand
;;   :config
;;   (ivy-mode t)
;;   (setq-default ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
;;   :bind (("C-b" . switch-to-buffer)))

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
  :config
  (global-hungry-delete-mode t))

;; (use-package helm
;;   :demand
;;   :pin melpa-stable
;;   :config
;;   (helm-mode t)
;;   :bind (("C-b" . helm-mini)
;;          ("<escape>" . helm-mini)
;;          ("M-x" . helm-M-x)
;;          ("C-x C-f" . helm-find-files)
;;          :map helm-map
;;          ("<escape>" . helm-keyboard-quit)))

;; (use-package helm-projectile
;;   :demand
;;   :pin melpa-stable
;;   :bind (("C-p" . helm-projectile))
;;   :config
;;   (projectile-global-mode))

;; (use-package multiple-cursors
;;   :demand
;;   :pin melpa-stable
;;   :bind (("C-c C-a" . mc/edit-beginnings-of-lines)
;;          ("C-c C-e" . mc/edit-ends-of-lines)
;;          ("C-c C-n" . mc/mark-next-like-this)
;;          ("C-c C-p" . mc/unmark-next-like-this)))

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

(use-package solarized-theme
  :demand
  :config
  (load-theme 'solarized-light)
  ;; Set consistent weight.
  ;; (mapc
  ;;  (lambda (face)
  ;;    (set-face-attribute face nil :weight 'semi-bold :underline nil))
  ;;  (face-list))
  (set-face-attribute 'mode-line-inactive nil
                      :underline nil
                      :overline nil
                      :box '(:line-width 2 :style released-button)
                      :height 0.7)
  (set-face-attribute 'mode-line nil
                      :underline nil
                      :overline nil
                      :box '(:line-width 2 :style released-button)
                      :height 0.7)
  ;; Customizations for helm colours.
  ;; (set-face-attribute 'helm-selection nil
  ;;                     :background "#FF6E64"
  ;;                     :underline nil)
  ;; (set-face-attribute 'helm-source-header nil
  ;;                     :background "#3F4D91")
  ;; Make the cursor red.
  ;; Doesn't work for future frames?
  ;; (set-face-attribute 'cursor nil :background "red")
  )

(use-package bm
  :demand
  :bind (("M-." . bm-next)
         ("M-," . bm-previous)
         ("<M-SPC>" . bm-toggle))
  :config
  ;; Should perhaps go in solarized config?
  (set-face-attribute 'bm-face nil
                      :underline nil
                      :overline nil
                      :background "yellow"))

(use-package emojify
  :demand
  :config
  (global-emojify-mode)
  (setq emojify-display-style 'image)) ;; 😊😍😡😲😳😷❤🚗🌛🌱🍃🍄

;; (use-package company-emoji
;;   :demand
;;   :config
;;   (company-emoji-init))

(use-package flycheck)

(use-package magit)

(use-package transpose-frame
  :bind ("C-x t" . transpose-frame))

(use-package git-gutter+)

(use-package visual-regexp)

(use-package rainbow-mode)

(use-package feature-mode)

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package markdown-mode)
