(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'highlight-symbol)
(require 'package)

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
(setq-default mode-line-format (list "%Z %6l %2c >>> %m; %b; %f; %P"))
(setq show-help-function nil)
(show-paren-mode)
(setq show-paren-style 'expression)
(set-default 'truncate-lines t)

;; Stop using bold fonts
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

 ;; Get rid of disgusting 3D styling
(set-face-attribute 'mode-line-inactive nil :box t)
(set-face-attribute 'mode-line nil :box t)

(when (display-graphic-p)
  (set-default-font "8"))

(setf text-scale-mode-step 1.05)

(load-theme 'solarized-light)

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

;; TODO Does this even work?
(setf arm 'auto-revert-mode)

(defun gen-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(windmove-default-keybindings)
(ido-mode)
(cua-mode)
  
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'beginning)
    (define-key map (kbd "C-e") 'ending)
    (define-key map (kbd "C-d") 'kill-whole-line)
    (define-key map (kbd "C-/") 'mc/edit-lines)
    (define-key map (kbd "C-n") 'mc/mark-next-like-this)
    (define-key map (kbd "C-p") 'mc/unmark-next-like-this)
    (define-key map (kbd "C-f") 'highlight-symbol-at-point)
    (define-key map (kbd "C-.") 'highlight-symbol-next)
    (define-key map (kbd "C-,") 'highlight-symbol-prev)
    (define-key map (kbd "C--") 'text-scale-decrease)
    (define-key map (kbd "C-=") 'text-scale-increase)
    map)
  "")

(define-minor-mode my-keys-minor-mode
  "Minor mode for my keys."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(add-hook 'after-load-functions 'my-keys-have-priority)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes. Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(server-start)

