(tool-bar-mode -1)

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
(define-key paredit-mode-map (kbd "C-<backspace>") 'backward-kill-sexp) ;; this one seems to be missing and results in broken syntax
