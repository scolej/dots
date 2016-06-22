(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; Available in stable.
(require 'auto-complete)
(require 'back-button)
(require 'company)
(require 'company)
(require 'drag-stuff)
(require 'expand-region)
(require 'helm-config)
(require 'jump-char)
(require 'multiple-cursors)
(require 'mwim)
(require 'projectile)

;; Not available in stable.
(require 'helm-projectile)
(require 'hungry-delete)
(require 'duplicate-thing)
(require 'highlight-thing)

(setq revert-without-query '(".*"))
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default dired-listing-switches "-alh")
(setq-default truncate-partial-width-windows nil)
(setq-default sr-speedbar-auto-refresh nil)
;; (add-to-list 'warning-suppress-types '(undo discard-info))
(setq-default comint-scroll-show-maximum-output nil)
(setq-default company-minimum-prefix-length 2)
(global-set-key (kbd "M-z") 'jump-char-forward)
;; (define-key shell-mode-map (kbd "TAB") #'company-manual-begin)

(load-theme 'solarized-light)
(add-to-list 'default-frame-alist '(cursor-color . "red"))
(set-face-attribute 'mode-line-inactive nil :box t :underline nil :overline nil :height 0.7)
(set-face-attribute 'mode-line nil :box t :underline nil :overline nil :height 0.7)

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
              save-interprogram-paste-before-kill t)

(setq-default mode-line-format (list "%Z %6l %2c > %m; %b; %f; %P"))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode t)
(blink-cursor-mode -1)
(back-button-mode t)
(global-company-mode)
(global-auto-complete-mode t)
(global-company-mode t)
(transient-mark-mode t)
(global-hungry-delete-mode t)
(setf text-scale-mode-step 1.05)
(windmove-default-keybindings)
(projectile-global-mode)
(recentf-mode t)
(helm-mode t)
(delete-selection-mode 1)
(windmove-default-keybindings)

;; Stop polluting the entire filesystem with backup files.
(if (boundp '*my-backup-dir*)
    (let ((dir *my-backup-dir*))
      (setq backup-directory-alist `((".*" . ,dir)))
      (setq auto-save-file-name-transforms `((".*" ,dir t)))))

(defun words-dammit ()
  (interactive)
  (toggle-truncate-lines)
  (visual-line-mode))

(defun arm ()
  (interactive)
  (auto-revert-mode))

;; From magnars/.emacs.d
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))
(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)

(global-set-key (kbd "C-`") 'ibuffer)
(global-set-key (kbd "C-a") 'mwim-beginning-of-line-or-code)
(global-set-key (kbd "C-c o") 'ffap)
(global-set-key (kbd "C-c r") 'revert-buffer)

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(global-set-key (kbd "C-p") 'helm-projectile)
(global-set-key (kbd "C-b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-/") 'mc/edit-lines)
(global-set-key (kbd "C-c a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/unmark-next-like-this)
(global-set-key (kbd "M-s s") 'sort-lines)

(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-d") 'duplicate-thing)
(global-set-key (kbd "M-u") 'er/expand-region)
(global-set-key (kbd "M-s s") 'sort-lines)

(global-set-key (kbd "<M-down>") 'drag-stuff-down)
(global-set-key (kbd "<M-up>") 'drag-stuff-up)

(global-set-key (kbd "<M-left>") 'back-button-local-backward)
(global-set-key (kbd "<M-right>") 'back-button-local-forward)

(global-set-key (kbd "<mouse-3>") 'kill-region)
(global-set-key (kbd "<mouse-2>") 'mouse-yank-at-click)
(setq-default select-active-regions nil)
(setq-default mouse-drag-copy-region t)

;; (define-minor-mode my-keys-minor-mode
;;   "Minor mode for my keys."
;;   :init-value t
;;   :lighter " my-keys"
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C--") 'text-scale-decrease)
;;             (define-key map (kbd "C-/") 'mc/edit-lines)
;;             (define-key map (kbd "C-=") 'text-scale-increase)
;;             (define-key map (kbd "C-`") 'ibuffer)
;;             (define-key map (kbd "C-a") 'mwim-beginning-of-line-or-code)
;;             (define-key map (kbd "C-b") 'ivy-switch-buffer)
;;             (define-key map (kbd "C-c a") 'mc/edit-beginnings-of-lines)
;;             (define-key map (kbd "C-c c") 'comment-region)
;;             (define-key map (kbd "C-c e") 'mc/edit-ends-of-lines)
;;             (define-key map (kbd "C-c n") 'mc/mark-next-like-this)
;;             (define-key map (kbd "C-c o") 'ffap)
;;             (define-key map (kbd "C-c p") 'mc/unmark-next-like-this)
;;             (define-key map (kbd "C-c u") 'uncomment-region)
;;             (define-key map (kbd "C-d") 'kill-whole-line)
;;             (define-key map (kbd "C-p") 'projectile-find-file)
;;             (define-key map (kbd "C-v") 'yank)
;;             (define-key map (kbd "C-z") 'undo)
;;             (define-key map (kbd "M-d") 'duplicate-thing)
;;             (define-key map (kbd "M-u") 'er/expand-region)
;;             ;; (define-key map (kbd "C-b") 'helm-mini)
;;             ;; (define-key map (kbd "C-x C-f") 'helm-find-files)
;;             ;; (define-key map (kbd "M-x") 'helm-M-x)
;;             map))

;; (my-keys-minor-mode t)

;; (defun my-keys-have-priority (_file)
;;   "Try to ensure that my keybindings retain priority over other minor modes. Called via the `after-load-functions' special hook."
;;   (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
;;     (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
;;       (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
;;       (add-to-list 'minor-mode-map-alist mykeys))))

;; (remove-hook 'after-load-functions 'my-keys-have-priority)
;; (add-hook 'after-load-functions 'my-keys-have-priority)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
