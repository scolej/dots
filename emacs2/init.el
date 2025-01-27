(tool-bar-mode -1)
(scroll-bar-mode -1)

;; remember minibuffer history between restarts
(savehist-mode 1)
(repeat-mode 1)
(delete-selection-mode 1)

(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)

(setq
 backup-directory-alist '((".*" . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 6
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t))
 auto-save-timeout 30
 auto-save-interval 0
 create-lockfiles nil)

(global-set-key (kbd "C-z") 'undo)

;; ruby stuff
(setq ruby-indent-level 4)

(defun beginning-of-line-toggle ()
  (interactive)
  (if (eq (- (point) (pos-bol)) (current-indentation))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "<home>") 'beginning-of-line-toggle)
(global-set-key (kbd "<end>") 'end-of-line)

;; tab bar
(global-set-key (kbd "s-t") 'tab-new)
(global-set-key (kbd "s-w") 'tab-close)
(global-set-key (kbd "C-<next>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-S-<next>") 'tab-bar-move-tab)
(global-set-key (kbd "C-S-<prior>") 'tab-bar-move-tab-backward)

(fset 'yes-or-no-p 'y-or-n-p)

;; dired
(require 'dired)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(define-key dired-mode-map (kbd "<backspace>") 'dired-jump)

(setq completion-auto-help 'always
      completion-auto-select 'second-tab)

;; clojure
(require 'cider)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode 'paredit-mode)
(define-key paredit-mode-map (kbd "C-<backspace>") 'backward-kill-sexp) ;; this one seems to be missing and results in broken syntax

(global-set-key (kbd "<escape>") 'keyboard-quit)

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(setq save-interprogram-paste-before-kill t)

(global-tab-line-mode 1)
(global-set-key (kbd "<tab-line> <wheel-up>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "<tab-line> <wheel-down>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "s-<left>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "s-<right>") 'tab-line-switch-to-next-tab)

(global-set-key (kbd "C-<tab>") 'other-window)

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-w") 'bury-buffer)
(global-set-key (kbd "s-/") 'comment-dwim)

(setq-default truncate-lines t)

(setq-default
 mode-line-format
 '((:eval (cond
           ((get-buffer-process (current-buffer))
            '(:propertize ">>>" face (:background "orange")))
           ((and (buffer-file-name) (buffer-modified-p))
            '(:propertize "+" face (:background "yellow")))))
   (:eval (or (buffer-file-name) (buffer-name)))
   ":%l:%c"))

;; ergonomic copy cut paste
(global-set-key (kbd "<S-return>") 'yank)
(global-set-key (kbd "<S-backspace>") 'kill-region)
(defun maybe-copy-region ()
  (interactive)
  (if (region-active-p)
      (yank))     )
