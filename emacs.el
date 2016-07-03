(setq debug-on-error t)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(require 'use-package)

(prefer-coding-system 'utf-8)

(setq use-package-always-ensure t)

(use-package markdown-mode)

(use-package ibuffer
  :config
  (setq-default ibuffer-default-sorting-mode '(filename/process)))

(use-package shell
  :config
  (setq-default comint-scroll-show-maximum-output nil))

(use-package sr-speedbar
  :config
  (setq-default sr-speedbar-auto-refresh nil
                sr-speedbar-width 20
                speedbar-show-unknown-files t
                speedbar-use-images t
                speedbar-vc-do-check nil)
  (add-hook 'speedbar-mode-hook (lambda () (text-scale-set -7)))
  :bind (("<f1>" . sr-speedbar-toggle)))

(use-package company
  :config
  (global-company-mode t)
  :bind (:map shell-mode-map ("<tab>" . company-complete)))

(use-package company-emoji
  :config
  (company-emoji-init))

(use-package drag-stuff
  :pin melpa-stable
  :bind (("<M-down>" . drag-stuff-down)
         ("<M-up>" . drag-stuff-up)))

(use-package expand-region
  :pin melpa-stable
  :bind (("M-u" . er/expand-region)))

(use-package helm
  :pin melpa-stable
  :config
  (helm-mode t)
  :bind (("C-b" . helm-mini)
         ("<escape>" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package helm-projectile
  :pin melpa-stable
  :bind (("C-p" . helm-projectile)))

(use-package multiple-cursors
  :pin melpa-stable
  :bind (("C-c a" . mc/edit-beginnings-of-lines)
         ("C-c e" . mc/edit-ends-of-lines)
         ("C-c n" . mc/mark-next-like-this)
         ("C-c p" . mc/unmark-next-like-this)
         ("M-s s" . sort-lines)))

(use-package mwim
  :pin melpa-stable
  :bind (("C-a" . mwim-beginning-of-line-or-code)))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode t))

(use-package duplicate-thing
  :bind (("M-d" . duplicate-thing)))

(use-package highlight-thing)

(use-package solarized-theme
  :config
  (load-theme 'solarized-light)
  (set-face-attribute 'mode-line-inactive nil
                      :underline nil
                      :overline t
                      :height 0.7)
  (set-face-attribute 'mode-line nil
                      :underline nil
                      :overline t
                      :height 0.7)
  (add-to-list 'default-frame-alist '(cursor-color . "red"))
  ;; Disable bold fonts.
  (mapc
   (lambda (face)
     (set-face-attribute face nil :weight 'normal :underline nil))
   (face-list)))

(use-package emojify
  :config
  (global-emojify-mode)
  (setq emojify-display-style 'image)) ;; 😊😍😡😲😳😷❤🚗🌛🌱🍃🍄

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package flycheck)

(use-package nyan-mode)

(use-package magit)

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
              ;; mode-line-format (list "%Z %6l %2c > %m; %b; %f; %P")
              revert-without-query '(".*")
              dired-listing-switches "-alh"
              truncate-partial-width-windows nil
              split-height-threshold 1200
              split-width-threshold 2000)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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

;; Keys
(windmove-default-keybindings)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-`") 'ibuffer)
(global-set-key (kbd "C-c o") 'ffap)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'replace-string)
(global-set-key (kbd "<C-return>") 'set-rectangular-region-anchor)
(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(add-hook 'text-mode-hook 'words-dammit)
