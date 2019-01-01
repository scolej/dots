;; TODO
;; eval-sexp-or-region
;; Minor mode key map
;; C-up/down to fight Cabal-version
;; C-` toggle case

;; It would be really cool if when finding files with ivy, suggestions
;; went N deep from the current folder; counsel-jump* is fine, but no
;; good for large dirs.

;; What about, when ever you make a selection, it gets copied; then,
;; only if you delete it, that entry is popped off.

(load "handy.el")

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure nil)

(setq-default c-basic-offset 4
              cursor-type 'box
              dired-listing-switches "-alh"
              dired-auto-revert-buffer t
              dired-dwim-target nil
              dired-recursive-deletes 'always
              dired-clean-confirm-killing-deleted-buffers nil
              hi-lock-auto-select-face t
              indent-tabs-mode nil
              inhibit-startup-message t
              initial-scratch-message nil
              isearch-allow-scroll nil
              isearch-wrap-function '(lambda nil)
              large-file-warning-threshold nil
              lazy-highlight-cleanup t
              lazy-highlight-max-at-a-time nil
              linum-format "%4d"
              mode-line-format '((:eval (if (get-buffer-process (current-buffer)) '(:propertize ">>>" face success) "%*"))  " %b")
              mouse-autoselect-window 0.2
              mouse-wheel-progressive-speed nil
              mouse-wheel-scroll-amount '(4 ((shift) . 4))
              recentf-max-saved-items 100
              revert-without-query '(".*")
              ring-bell-function 'ignore
              save-interprogram-paste-before-kill t
              sentence-end-double-space nil
              set-mark-command-repeat-pop t
              scroll-conservatively 0
              scroll-margin 0
              show-paren-style 'parenthesis
              show-trailing-whitespace t
              tab-width 4
              truncate-lines t
              truncate-partial-width-windows nil
              visible-bell nil
              frame-title-format '("%b")
              load-prefer-newer t
              split-height-threshold 150
              split-width-threshold 200
              even-window-heights nil
              next-error-recenter nil)

(advice-add 'help-window-display-message :around #'ignore)

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

(global-set-key (kbd "<escape>") 'execute-extended-command)

(winner-mode t)
(global-set-key (kbd "<M-left>") 'winner-undo)
(global-set-key (kbd "<M-right>") 'winner-redo)

;; FIXME
(defun isearch-with-region ()
  (interactive)
  (if mark-active
      (let ((region (funcall region-extract-function nil)))
        (goto-char (region-beginning))
        (deactivate-mark)
        (isearch-update)
        (isearch-yank-string region))
    (call-interactively 'isearch-forward)))

(global-set-key (kbd "C-s") 'isearch-forward)


;; FIXME Does anything?
;; (setq display-buffer-alist '((".*" . (display-buffer-pop-up-window . ((inhibit-same-window . t) ;
;;                                                                       (inhibit-switch-frame . t))))))
;; (setq display-buffer-alist nil)

(blink-cursor-mode 0)
(delete-selection-mode t)
(electric-indent-mode t)
(fringe-mode nil)
(global-eldoc-mode -1)
(global-hl-line-mode 0)
(global-subword-mode t)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode t)
(tool-bar-mode 0)
(transient-mark-mode t)

(tooltip-mode 0)
(setq show-help-function nil)

(add-hook 'comint-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
(add-hook 'occur-hook #'occur-rename-buffer)
(add-hook 'occur-hook #'hl-line-mode)
(add-hook 'archive-mode-hook #'hl-line-mode)

;; (global-set-key (kbd "<escape>") #'top-level)

;; (global-set-key "\t" #'other-window)
(global-set-key (kbd "<tab>") #'other-window)
(global-set-key (kbd "<S-tab>") (lambda () (interactive) (other-window -1)))
;; (global-set-key (kbd "<S-tab>") nil)
;; (global-set-key "S\t" #'other-window)

(global-set-key (kbd "<S-next>") #'chunky-scroll-left)
(global-set-key (kbd "<S-prior>") #'chunky-scroll-right)
(global-set-key (kbd "<f5>") #'revert-buffer)
(global-set-key (kbd "<wheel-left>") #'small-scroll-right)
(global-set-key (kbd "<wheel-right>") #'small-scroll-left)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-vertically)
(global-set-key (kbd "C-3") #'split-window-horizontally)
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-\\") #'replace-string)
(global-set-key (kbd "C-c b b") #'copy-buffer-path)
(global-set-key (kbd "C-c b l") #'copy-buffer-path-and-line)
(global-set-key (kbd "C-c f c") #'make-frame)
(global-set-key (kbd "C-c f d") #'delete-frame)
(global-set-key (kbd "C-c l") #'list-processes)
(global-set-key (kbd "C-c r") #'revert-buffer)
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-x <down>") #'windmove-down)
(global-set-key (kbd "C-x <left>") #'windmove-left)
(global-set-key (kbd "C-x <right>") #'windmove-right)
(global-set-key (kbd "C-x <up>") #'windmove-up)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x k") #'really-kill-buffer)
(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "M-s d") #'delete-trailing-whitespace)
(global-set-key (kbd "M-s s") #'sort-lines)
(global-set-key (kbd "M-s u") #'upcase-region)
(global-set-key [S-wheel-down] #'chunky-scroll-left)
(global-set-key [S-wheel-up] #'chunky-scroll-right)

(global-set-key (kbd "<f12>") #'recompile)

;; Unmap shenanigans.
(global-set-key (kbd "<f2>") nil)

(defun close-window-or-frame ()
  (interactive)
  (if (one-window-p) (delete-frame) (delete-window)))
(global-set-key (kbd "<f19>") #'close-window-or-frame)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "M-S-y") 'yank-pop-forwards)

(defun define-keys (keymap &rest keys)
  "Make multiple bindings in a map."
  (cl-loop for (key binding) on keys by #'cddr do
           (define-key keymap (kbd key) binding)))

(defun keymap (&rest bindings)
  "Make a new keymap with bindings. Return that map."
  (let ((map (make-sparse-keymap)))
    (apply #'define-keys map bindings)
    map))

;;
;; Built-ins
;;

(require 'help-mode)
(define-key help-mode-map (kbd "n") 'forward-button)
(define-key help-mode-map (kbd "p") 'backward-button)

(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)
(define-key occur-mode-map (kbd "o") 'occur-mode-display-occurrence)

(cua-mode)
(define-key cua-global-keymap (kbd "C-x C-x") nil) ;; FIXME Doesn't work?
(setq cua-auto-mark-last-change t)

(require 'dired)
(defun dired-display-end-of-file ()
  (interactive)
  (with-selected-window (dired-display-file)
    (end-of-buffer)))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(define-keys dired-mode-map
  "<backspace>" #'dired-up-directory
  "C-t" nil
  "o" 'dired-display-file
  "O" 'dired-display-end-of-file)

(add-hook 'ibuffer-mode-hook #'hl-line-mode)

(require 'dired-x)
(global-set-key (kbd "M-j") #'dired-jump)

(require 'cc-mode)
(define-key java-mode-map (kbd "C-d") nil)

;;
;; Misfits
;;

(require 'save-all-the-things)
(add-hook 'find-file-hook #'save-all-the-things-mode)

(require 'co-man-der)
(require 'pikatock)

;;
;; Packages
;;

(use-package bm
  :config
  (setq bm-wrap-search nil)
  :bind (("<C-next>" . #'bm-next)
         ("<C-prior>" . #'bm-previous)))

(use-package counsel)

(use-package expand-region
  :bind (("M-u" . #'er/expand-region)))

(use-package mwim
  ;; TODO how to make this work with visual line mode??
  :bind (("C-a" . #'mwim-beginning-of-code-or-line)))

(use-package ivy
  :demand
  :config

  (defun ivy-insert-or-expand-dir ()
    "Insert the current candidate into current input.
Don't finish completion. If input matches is a directory,
use it to continue completion.
FIXME Do we really need this? Is it not the default?"
    (interactive)
    (ivy-insert-current)
    (when (setq dir (ivy-expand-file-if-directory (ivy--input)))
      (ivy--cd dir)))

  (defun cancel-region-or-switch-buffer ()
    (interactive)
    (if (region-active-p) (keyboard-quit)
      (ivy-switch-buffer)))

  (setq-default ivy-use-virtual-buffers nil
                ivy-do-completion-in-region nil
                ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  (ivy-mode t)

  :bind (("<f1>" . 'ivy-switch-buffer)
         ;;("<f3>" . 'ace-jump-buffer)
         ("C-c C-r" . 'ivy-resume)
         :map ivy-minibuffer-map
         ("<f1>" . 'ivy-next-line)
         ("<f2>" . 'ivy-previous-line)
         ("<f3>" . 'ivy-done)
         ("<escape>" . minibuffer-keyboard-quit)
         ;; This is the default binding but global tab binding elsewhere means we need this.
         ("<tab>" . 'ivy-partial-or-done)
         ))

(use-package avy
  :bind (("<f13>" . 'avy-goto-char)))

;; (use-package ace-jump-buffer
;;   :bind (("<f1>" . 'ace-jump-buffer)))

(use-package ace-jump-buffer
  :bind (("<f1>" . 'ace-jump-buffer)))

(use-package swiper
  :config
  (defun smart-swiper ()
    (interactive)
    (let ((initial (if (region-active-p) (buffer-substring-no-properties (mark) (point)) "")))
      (deactivate-mark)
      (swiper initial)))
  :bind (;; ("C-o" . #'smart-swiper)
         ))

(use-package drag-stuff
  :bind (("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

(use-package duplicate-thing
  ;; FIXME at end of file :(
  ;; FIXME don't save region, I almost never want to paste again what I just duplicated.
  :init
  (defun duplicate-thing-down ()
    (interactive)
    (duplicate-thing 1)
    (next-line))
  :bind (("<C-M-up>" . duplicate-thing)
         ("<C-M-down>" . duplicate-thing-down)))

(use-package ag
  :config
  (defun ag-here (str)
    (interactive "M")
    (ag str default-directory))
  :bind (("C-x a" . #'ag-here)
         :map ag-mode-map
         ("o" . 'compilation-display-error)))

(use-package highlight-thing
  :config
  (setq highlight-thing-what-thing nil
        highlight-thing-prefer-active-region t
        highlight-thing-exclude-thing-under-point t)
  (global-highlight-thing-mode t))

(use-package groovy-mode)

(use-package nxml-mode
  :config
  (setq nxml-child-indent 4
        nxml-attribute-indent 4))

(use-package projectile
  :demand
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy
        projectile-indexing-method 'alien
        projectile-switch-project-action 'projectile-dired)
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "bin")
  :bind (("C-c p" . 'projectile-command-map)
         ("<S-escape>" . 'projectile-find-file)
         ("<f2>" . 'projectile-find-file)
         ("<S-f2>" . 'projectile-switch-to-buffer)))

;;
;; Super awesome nested handy-map.
;;

(defvar quick-sites-map (make-sparse-keymap)
  "Keymap for quickly opening URLs.")

(define-keys minibuffer-local-map
  ;; Need this? "<escape>" #'top-level
  "\\" #'self-insert-command)

(global-set-key
 (kbd "\\")
 (keymap
  "E" #'eval-print-last-sexp
  "\\" #'self-insert-command
  "c" #'make-frame
  "e" #'eval-last-sexp
  "f" #'find-file
  "F" #'counsel-file-jump
  "g" #'maybe-google-region
  "h" #'please-help-me
  "k" #'really-kill-buffer
  "m" #'jump-to-commands
  "m" #'quickbuf-commands
  "o" #'try-find-file
  "q" #'quit-window
  "r" #'moss-speedy-rerun
  "s" #'quickbuf-handy
  "w" #'delete-trailing-whitespace
  "i" quick-sites-map
  ))


(define-derived-mode wrapping-text-mode fundamental-mode
  (toggle-truncate-lines -1)
  (visual-line-mode t))
(add-to-list 'auto-mode-alist '("\\.tx\\'" . wrapping-text-mode))
