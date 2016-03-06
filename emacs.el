;; https://www.emacswiki.org/emacs/SiteMap

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4f81886421185048bd186fbccc98d95fca9c8b6a401771b7457d81f749f5df75" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setf package-archives '())
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp")

;; (require 'adaptive-wrap)
;; Use by M-x adaptive-adaptive-wrap-prefix-mode

(blink-cursor-mode 0)
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)))
(setq mouse-wheel-progressive-speed nil)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; (set-default-font "Terminus 10")
;; (set-default-font "7x13")
(set-default-font "Menlo 11")
(setq mouse-autoselect-window t)
(windmove-default-keybindings)
(global-auto-revert-mode t)
;; (load-theme 'leuven)
(load-theme 'solarized-dark)
;; (load-theme 'solarized-light)
;; (load-theme 'autumn-light)
;; (global-hl-line-mode 0)
(show-paren-mode t)
(ido-mode)
(setq-default indent-tabs-mode nil)
(cua-mode)
(global-linum-mode)
(setq linum-format "%4d")
;; (setq-default mode-line-format nil)
(setq-default mode-line-format (list ">>> %m; %b; %f"))
(setq show-help-function nil)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (setq-default 'haskell-indent-offset 2)

(add-hook 'buffer-menu-mode-hook 'pm)

(global-set-key (kbd "C-`") 'buffer-menu)
(global-set-key (kbd "M-r") 'replace-regexp)
(global-set-key (kbd "M-l") 'recenter-top-bottom)

(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-set-key (kbd "C-n") 'highlight-symbol-next)
(global-set-key (kbd "C-S-n") 'highlight-symbol-prev)

;; Select current line
;; (global-set-key (kbd "C-l")
;;                 (lambda ()
;;                   (interactive)
;;                   (move-beginning-of-line nil)
;;                   (cua-set-mark)
;;                   (move-end-of-line nil)))

(global-set-key (kbd "M-s") 'exchange-point-and-mark)

;; Multi cursor bindings
(global-set-key (kbd "M-a") 'mc/edit-ends-of-lines)
(global-set-key (kbd "M-i") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "M-m") 'mc/edit-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/unmark-next-like-this)

(require 'expand-region)
(global-set-key (kbd "<M-up>") 'er/expand-region)

(setq lazy-highlight-cleanup nil)

;; Try and get the escape key doing more C-g like stuff.
(define-key isearch-mode-map [escape] 'isearch-abort)
(define-key isearch-mode-map "\e" 'isearch-abort)
(define-key Buffer-menu-mode-map [escape] 'quit-window)
(global-set-key [escape] 'keyboard-escape-quit)

;; (defun backward-whitespace () (interactive) (forward-whitespace -1))
;; (global-set-key (kbd "C-<right>") 'forward-whitespace)
;; (global-set-key (kbd "C-<left>") 'backward-whitespace)

;; Set up for writing words
(defun wm ()
  (interactive)
  (font-lock-mode 0)
  (visual-line-mode t)
  (linum-mode 0)
  (hl-line-mode 0))

;; Set up for writing code
(defun pm ()
  (interactive)
  (font-lock-mode t)
  (visual-line-mode 0)
  (toggle-truncate-lines t)
  (linum-mode t)
  (hl-line-mode t))

(set-face-attribute 'mode-line          nil :box nil :underline nil :overline nil)
(set-face-attribute 'mode-line-inactive nil :box nil :underline nil :overline nil)

(global-set-key (kbd "C-o") 'ffap)

;; Reminders to try later
;; delete-trailing-whitespace
;; C-x C-o remove double blank lines (delete-blank-lines)
;; M-x highlight-regexp
;; M-x unhighlight-regexp
;; Inserting random chars
;;        C-q C-[ escape

(setq inferior-lisp-program "/usr/local/bin/sbcl")
