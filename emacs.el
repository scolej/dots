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
              lazy-highlight-cleanup t
              lazy-highlight-max-at-a-time nil
              show-trailing-whitespace nil
              scroll-conservatively 1000
              scroll-margin 0
              recentf-max-saved-items 100)

;; Make the cursor red in future frames. TODO :/ Doesn't work ??
;; Works if you don't call (set-face-attribute) for the cursor ??
(add-to-list 'default-frame-alist '(cursor-color . "red"))

;; Experiment with facy mode-line...
(defun save-smiley()
  (cond
   ((and (buffer-modified-p) buffer-read-only) ":{O")
   (buffer-read-only ":{")
   ((buffer-modified-p) ":O")
   ((not (buffer-modified-p)) ":)")
   t ":S"))
(defun generate-modeline ()
  (string-join (list (save-smiley)
                     (format "%4d:%2d" (line-number-at-pos (point)) (current-column))
                     (format "%3d%%%%" (/ (point) 0.01 (point-max)))
                     (buffer-name)
                     ;;(buffer-file-name)
                     )
               " "))
(setq-default mode-line-format '("" (:eval (generate-modeline))))
;; TODO Is this overkill?
(add-hook 'post-command-hook 'force-mode-line-update)

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
(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)

;; Speedy grep
;; Provide an rgrep which will use the last values unless there is a prefix arg to specify them again.
(defvar speedy-grep-last-glob nil)
(defvar speedy-grep-last-dir nil)
(defun speedy-grep-rgrep (glob dir pat)
  (interactive
   (list
    (if (or (not speedy-grep-last-glob)
            current-prefix-arg)
        (completing-read "Glob: " nil 'glob)
      speedy-grep-last-glob)
    (if (or (not speedy-grep-last-dir)
            current-prefix-arg)
        (read-directory-name "Directory: ")
      speedy-grep-last-dir)
   (completing-read "Pattern: " nil 'pat)))
  (progn
    (setq speedy-grep-last-dir dir)
    (setq speedy-grep-last-glob glob)
    (rgrep pat glob dir)))

;; Functions and bindings for long-pressing right mouse button for copy / cut.
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
(global-set-key (kbd "M-r") 'speedy-grep-rgrep)
(global-set-key (kbd "M-s s") 'sort-lines)
(global-set-key (kbd "M-s d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key (kbd "<C-tab>") 'mode-line-other-buffer)
(global-set-key (kbd "C-x f") 'recentf-open-files)
(global-set-key [S-wheel-down] 'scroll-left)
(global-set-key [S-wheel-up] 'scroll-right)

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

(use-package ace-jump-mode
  :demand
  :bind (("<M-return>" . ace-jump-char-mode)))

(use-package projectile
  :config
  (projectile-global-mode t)
  (setq-default projectile-indexing-method 'alien
                projectile-completion-system 'ivy)
  :bind (("C-p" . projectile-find-file)
         ("C-S-p" . projectile-switch-project)))

(use-package swiper
  :demand
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
  :bind (("<S-backspace>" . hungry-delete-backward)
         ("<S-delete>" . hungry-delete-forward)))

(use-package mwim
  :demand
  :pin melpa-stable
  :bind (("C-a" . mwim-beginning-of-code-or-line)))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-mode t))

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
  (global-emojify-mode t)
  (setq emojify-display-style 'image)) ;; 😊😍😡😲😳😷❤🚗🌛🌱🍃🍄

(use-package flycheck)

(use-package magit
  :config
  (setq-default auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p
                vc-handled-backends nil))

(use-package transpose-frame
  :bind ("C-x t" . transpose-frame))

(use-package git-gutter+)

(use-package visual-regexp)

(use-package rainbow-mode)

(use-package feature-mode)

(use-package haskell-mode)

(use-package markdown-mode)
