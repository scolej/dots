(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'drag-stuff)
(require 'duplicate-thing)
(require 'expand-region)
;; (require 'helm-config)
(require 'hungry-delete)
(require 'mwim)
(require 'neotree)
(require 'back-button)
(require 'swiper)
(require 'highlight-thing)
(require 'fixme-mode)
(require 'auto-complete)

;; Disable annoying things
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)))
(setq mouse-wheel-progressive-speed nil)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(setq linum-format "%4d")
(setq-default mode-line-format (list "%Z %6l %2c > %m; %b; %f; %P"))
(setq show-help-function nil)
(show-paren-mode)
(setq show-paren-style 'expression)
(set-default 'truncate-lines t)
(blink-cursor-mode -1)

 ;; Get rid of disgusting 3D styling
(set-face-attribute 'mode-line-inactive nil :box t)
(set-face-attribute 'mode-line nil :box t)

(setq neo-theme 'ascii)
(add-to-list 'neo-hidden-regexp-list "\\.hi$")
(add-to-list 'neo-hidden-regexp-list "\\.o$")

(load-theme 'solarized-light)
(add-to-list 'default-frame-alist '(cursor-color . "red"))

(global-hl-line-mode)
(global-highlight-thing-mode)

(back-button-mode t)
(ac-config-default)
(transient-mark-mode t)
(global-hungry-delete-mode)
(setf text-scale-mode-step 1.05)
(windmove-default-keybindings)
(drag-stuff-global-mode)
(fixme-mode)
;; (ido-mode)

(ivy-mode)
(setq projectile-completion-system 'ivy)

;; No tabs!!
(setq-default indent-tabs-mode nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(delete-selection-mode 1)

;; Stop polluting the entire filesystem with backup files
(if (boundp '*my-backup-dir*)
    (let ((dir *my-backup-dir*))
      (setq backup-directory-alist `((".*" . , dir)))
      (setq auto-save-file-name-transforms `((".*" , dir t)))))

(defun words-dammit ()
  (interactive)
  (toggle-truncate-lines)
  (visual-line-mode))

(defun arm ()
  (interactive)
  (auto-revert-mode))

(define-minor-mode my-keys-minor-mode
  "Minor mode for my keys."
  :init-value t
  :lighter " my-keys"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C--") 'text-scale-decrease)
            (define-key map (kbd "C-/") 'mc/edit-lines)
            (define-key map (kbd "C-=") 'text-scale-increase)
            (define-key map (kbd "C-`") 'ibuffer)
            (define-key map (kbd "C-a") 'mwim-beginning-of-line-or-code)
            (define-key map (kbd "C-b") 'ivy-switch-buffer)
            (define-key map (kbd "C-c a") 'mc/edit-beginnings-of-lines)
            (define-key map (kbd "C-c c") 'comment-region)
            (define-key map (kbd "C-c e") 'mc/edit-ends-of-lines)
            (define-key map (kbd "C-c n") 'mc/mark-next-like-this)
            (define-key map (kbd "C-c o") 'ffap)
            (define-key map (kbd "C-c p") 'mc/unmark-next-like-this)
            (define-key map (kbd "C-c u") 'uncomment-region)
            (define-key map (kbd "C-d") 'kill-whole-line)
            (define-key map (kbd "C-p") 'projectile-find-file)
            (define-key map (kbd "C-s") 'swiper)
            (define-key map (kbd "C-r") 'swiper)
            (define-key map (kbd "C-v") 'yank)
            (define-key map (kbd "C-z") 'undo)
            (define-key map (kbd "M-d") 'duplicate-thing)
            (define-key map (kbd "M-u") 'er/expand-region)
            ;; (define-key map (kbd "C-b") 'helm-mini)
            ;; (define-key map (kbd "C-x C-f") 'helm-find-files)
            ;; (define-key map (kbd "M-x") 'helm-M-x)
            map))

(my-keys-minor-mode t)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes. Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(remove-hook 'after-load-functions 'my-keys-have-priority)
(add-hook 'after-load-functions 'my-keys-have-priority)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
