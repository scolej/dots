(setq-default c-basic-offset 4
              cursor-type 'box
              dired-listing-switches "-alh"
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
              mouse-wheel-progressive-speed nil
              mouse-wheel-scroll-amount '(4 ((shift) . 4))
              recentf-max-saved-items 100
              revert-without-query '(".*")
              ring-bell-function 'ignore
              save-interprogram-paste-before-kill t
              scroll-conservatively 9999
              scroll-margin 0
              show-help-function nil
              show-paren-style 'expression
              show-trailing-whitespace nil
              tab-width 4
              truncate-lines t
              truncate-partial-width-windows nil
              visible-bell nil)

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

(global-set-key (kbd "C-c f c") 'make-frame)
(global-set-key (kbd "C-c f d") 'delete-frame)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-s s") 'sort-lines)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key [S-wheel-down] '(lambda () (interactive) (scroll-left 5)))
(global-set-key [S-wheel-up] '(lambda () (interactive) (scroll-right 5)))

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "M-S-y") 'yank-pop-forwards)

(require 'dired)
(define-key dired-mode-map (kbd "<escape>") 'dired-up-directory)

(require 'dired-x)
(global-set-key (kbd "<escape>") 'dired-jump)

(require 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning-of-line-or-code)

(require 'magit)
(setq-default auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffers-p
              vc-handled-backends nil)
(add-hook 'magit-status-mode-hook 'delete-other-windows)
(global-set-key (kbd "C-c m") 'magit-status)

(load-theme 'acme)
