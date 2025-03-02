;;
;; Handier binding
;;

(defun define-keys (keymap &rest keys)
  "Make multiple bindings in a map."
  (cl-loop for (key binding) on keys by #'cddr do
           (define-key keymap (kbd key) binding)))

(defun keymap (&rest bindings)
  "Make a new keymap with bindings. Return that map."
  (let ((map (make-sparse-keymap)))
    (apply 'define-keys map bindings)
    map))

;;

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; remember minibuffer history between restarts
(savehist-mode 1)
(repeat-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(setq-default truncate-lines t)

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

(setq sentence-end-double-space nil)

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(setq-default
 auto-hscroll-mode t
 hscroll-step 10
 hscroll-margin 2)

(setq-default save-silently t)

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "<escape>") 'keyboard-quit)

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

(use-package tab-line
  :config
  (global-tab-line-mode 1)
  (setq tab-line-close-button nil)
  (global-set-key (kbd "C-<tab>") 'next-buffer)
  (global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer))

(fset 'yes-or-no-p 'y-or-n-p)

;; dired
(require 'dired)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(define-key dired-mode-map (kbd "<backspace>") 'dired-jump)

;;
;; completion
;;

(setq completion-styles '(partial-completion))

(setq completion-auto-help 'always
      completion-auto-select 'second-tab)

;; eliminates annoying delay when entering on sole completion
(setq completion-show-inline-help nil)

;;
;; mode-line
;;

(setq-default
 mode-line-format
 '((:eval (cond
           ((get-buffer-process (current-buffer))
            '(:propertize ">>>" face (:background "orange")))
           ((and (buffer-file-name) (buffer-modified-p))
            '(:propertize "+" face (:background "yellow")))))
   (:eval (or (buffer-file-name) (buffer-name)))
   ":%l:%c"))
(set-face-attribute 'mode-line nil :height 0.7)
(set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)

(set-face-attribute 'tab-line nil :height 0.8)
(set-face-attribute 'tab-bar nil :height 0.9)

;;
;;
;;

(use-package cider
  :ensure)

(defun insert-lambda ()
  (interactive)
  (insert "(λ ())")
  (forward-char -2))

(defun clone-sexp ()
  (interactive)
  (let* ((b (point))
         (e (save-excursion
              (forward-sexp)
              (point)))
         (s (buffer-substring-no-properties b e)))
    (insert s)
    (newline-and-indent)))

(use-package paredit
  :ensure
  :bind (:map
	 paredit-mode-map
	 ;; this one seems to be missing and results in broken syntax
	 ("C-<backspace>" . 'backward-kill-sexp)
	 ("[" . 'paredit-open-round)
	 ("]" . 'paredit-close-round)
	 ("M-c" . 'clone-sexp)
         ("C-\\" . 'insert-lambda)
	 ("{" . 'paredit-wrap-round)
	 ("}" . 'paredit-wrap-round-backwards)
)

  :hook emacs-lisp-mode
  :hook clojure-mode
  :hook scheme-mode)
;; todo binding for dragging s-exps forward and back

;; ruby stuff
(setq ruby-indent-level 4)

(load "idle-highlight.el")

;; sane scrolling
(setq mouse-wheel-scroll-amount '(2)
      mouse-wheel-progressive-speed nil)

(defun wip-it-good ()
  (interactive)
  (let ((dir (locate-dominating-file default-directory "test.sh")))
    (or dir (error "couldn't find wip script"))
    (message "running script")
    (start-process "*wip*" "*wip*" (file-name-concat dir "test.sh"))))
(global-set-key (kbd "<f11>") 'wip-it-good)

;;
;; mega leader map again
;;

(global-set-key
 "`"
 (keymap
  "`" 'self-insert-command
  "f" 'find-file
  "q" 'bury-buffer
  "k" 'kill-current-buffer
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right
  ))

;;

;; make auto-revert revert faster!
(setq
 auto-revert-interval 0.5
 auto-revert--lockout-interval 0.2)

;; dirty hack to try to stop weird behaviour i'm seeing when auto-revert combined with compilation-mode
;; not sure that it does something yet...
(defun maybe-forget-errors ()
  (when (equal major-mode 'compilation-mode)
    (compilation-forget-errors)))
(add-hook 'after-revert-hook 'maybe-forget-errors)

;;

(mapc
 (lambda (s) (add-to-list 'yank-excluded-properties s))
 '(face font-lock-face))
