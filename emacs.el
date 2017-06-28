(setq-default c-basic-offset 4
              cursor-type 'box
              dired-listing-switches "-alh"
              dired-auto-revert-buffer t
              hi-lock-auto-select-face t
              indent-tabs-mode nil
              inhibit-startup-message t
              initial-scratch-message nil
              isearch-allow-scroll t
              isearch-wrap-function '(lambda nil)
              large-file-warning-threshold nil
              lazy-highlight-cleanup t
              lazy-highlight-max-at-a-time nil
              linum-format "%4d"
              mouse-autoselect-window 0.5
              mouse-wheel-progressive-speed nil
              mouse-wheel-scroll-amount '(4 ((shift) . 4))
              recentf-max-saved-items 100
              revert-without-query '(".*")
              ring-bell-function 'ignore
              save-interprogram-paste-before-kill t
              sentence-end-double-space nil
              set-mark-command-repeat-pop t
              scroll-conservatively 9999
              scroll-margin 0
              show-help-function nil
              show-paren-style 'expression
              show-trailing-whitespace nil
              tab-width 4
              truncate-lines t
              truncate-partial-width-windows nil
              visible-bell nil)

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
(global-hl-line-mode 0)
(cua-mode 0)

(defun ttl ()
  "Shortcut for truncating lines."
  (interactive)
  (toggle-truncate-lines))

(defun words-dammit ()
  "I just want word wrapping!"
  (interactive)
  (toggle-truncate-lines 0)
  (visual-line-mode t))

(defun copy-buffer-path ()
  "Copy the full path to the current buffer's file."
  (interactive)
  (kill-new (buffer-file-name)))

(defun save-all ()
  "Save every buffer."
  (interactive)
  (save-some-buffers 'no-confirm))

(add-hook 'occur-hook 'occur-rename-buffer)
(add-hook
 'find-file-hook
 (lambda ()
   (when (string= (file-name-extension buffer-file-name) "log")
     (read-only-mode t)
     (auto-revert-mode t))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "C-c f c") 'make-frame)
(global-set-key (kbd "C-c f d") 'delete-frame)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-s s") 'sort-lines)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key [S-wheel-down] '(lambda () (interactive) (scroll-left 20)))
(global-set-key [S-wheel-up] '(lambda () (interactive) (scroll-right 20)))
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-z") 'undo)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "M-S-y") 'yank-pop-forwards)

(require 'dired)
(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)

(require 'dired-x)
(global-set-key (kbd "C-x j") 'dired-jump)

(require 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning-of-line-or-code)

(require 'magit)
(setq-default auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p
              vc-handled-backends nil)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(global-set-key (kbd "C-c m") 'magit-status)

(require 'transpose-frame)
(global-set-key (kbd "C-x t") 'transpose-frame)

(require 'auto-complete)
(ac-config-default)
(setq ac-auto-show-menu nil
      ac-use-quick-help nil)
(define-key ac-completing-map (kbd "<down>") nil)
(define-key ac-completing-map (kbd "<up>") nil)
(define-key ac-completing-map (kbd "<RET>") nil)
(define-key ac-completing-map (kbd "<return>") nil)
(define-key ac-completing-map (kbd "<backtab>") 'ac-previous)

(require 'flycheck)
(global-flycheck-mode)

(require 'haskell-mode)

(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)

(require 'bm)
(global-set-key (kbd "<M-SPC>") 'bm-toggle)
(global-set-key (kbd "<M-up>") 'bm-previous)
(global-set-key (kbd "<M-down>") 'bm-next)

(require 'back-button)
(global-set-key (kbd "<M-left>") 'back-button-global-backward)
(global-set-key (kbd "<M-right>") 'back-button-global-forward)

(require 'markdown-mode)
;; Remove bindings that clash with bindings for back-button.
(define-key markdown-mode-map (kbd "<M-left>") nil)
(define-key markdown-mode-map (kbd "<M-right>") nil)

(require 'ivy)
(ivy-mode)
(setq-default ivy-use-virtual-buffers t)
(global-set-key (kbd "<escape>") 'ivy-switch-buffer)
(define-key ivy-switch-buffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

(require 'swiper)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-S-s") 'isearch-forward)

(require 'select-whole-lines)
(select-whole-lines-mode)

;;
;; Experiment with auto saving.
;;

(defvar-local save-all-the-things-timer nil
  "Timer for each buffer to automatically save itsself.")

(defvar save-all-the-things-delay 2
  "Time to wait after a change before saving (seconds).")

(defun save-all-the-things-timer-setter (x y z)
  "Reset any existing save timer and set a new one.
Args X Y and Z are unused."
  (when buffer-file-name
    (when save-all-the-things-timer
      (cancel-timer save-all-the-things-timer))
    (setq-local save-all-the-things-timer
                (run-at-time save-all-the-things-delay
                             nil
                             (lambda (buf)
                               (when (get-buffer buf)
                                 (with-current-buffer buf
                                   (save-buffer))))
                               (buffer-name)))))

(add-hook 'after-change-functions 'save-all-the-things-timer-setter)
