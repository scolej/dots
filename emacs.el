(custom-set-variables
 '(custom-safe-themes
   (quote
    ("a25c42c5e2a6a7a3b0331cad124c83406a71bc7e099b60c31dc28a1ff84e8c04" "4f81886421185048bd186fbccc98d95fca9c8b6a401771b7457d81f749f5df75" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))

(when (require 'package nil :noerror)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

;; Disable annoying things
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Visual set up
(setq linum-format "%4d")
(setq-default mode-line-format (list ">>> %m; %b; %f"))
(setq show-help-function nil)
(show-paren-mode)

;; From https://www.emacswiki.org/emacs/SetFonts
(require 'cl)
(defun font-candidate (&rest fonts)
     "Return existing font which first match."
     (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(let ((f (font-candidate "Menlo 11" "Consolas 11" "Courier New 11")))
  (set-default-font f)
  (add-to-list 'default-frame-alist `(font . ,f)))

(or (ignore-errors (load-theme 'white-sand) t)
    (ignore-errors (load-theme 'solarized-light) t))

(set-cursor-color "#ff0000")

;; No tabs!!
(setq-default indent-tabs-mode nil)

(ido-mode)

(setq mouse-autoselect-window t)
(windmove-default-keybindings)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(defun switch-to-buffer-menu ()
  (interactive)
  (let ((b (get-buffer "*Buffer List*")))
    (if b (progn
            (switch-to-buffer b)
            (revert-buffer))
      (buffer-menu))))
(global-set-key (kbd "C-`") 'switch-to-buffer-menu)
(add-hook 'buffer-menu-mode-hook (lambda () (hl-line-mode)))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(global-set-key (kbd "M-s") 'exchange-point-and-mark)

;; Multi cursor bindings
(when (require 'multiple-cursors nil :noerror)
  (global-set-key (kbd "C-c C-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c C-m") 'mc/edit-lines)
  (global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-p") 'mc/unmark-next-like-this))

(when (require 'expand-region nil :noerror)
  (global-set-key (kbd "M-u") 'er/expand-region))

;; Try and get the escape key doing more C-g like stuff.
(define-key isearch-mode-map [escape] 'isearch-abort)
(define-key isearch-mode-map "\e" 'isearch-abort)
(define-key Buffer-menu-mode-map [escape] 'quit-window)

(global-set-key (kbd "C-c C-o") 'ffap)

;; (global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

(when (require 'dired-details+ nil :noerror)
  (setf dired-details-propagate-flag t)
  (setf dired-details-hidden-string "")
  (add-hook 'dired-mode (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)))

;; Reminders to try later
;; delete-trailing-whitespace
;; C-x C-o remove double blank lines (delete-blank-lines)
;; M-x highlight-regexp
;; M-x unhighlight-regexp
;; Inserting random chars
;;        C-q C-[ escape
;; M-x find-named-dired

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-d") 'duplicate-line)

(setq ido-auto-merge-work-directories-length 5)

;; (defun copy-line ()
;;   (interactive)
;;   (kill-ring-save (line-beginning-position)
;;                   (line-beginning-position 1)))

(global-set-key (kbd "C-c C-c") (kbd "C-a <C-SPC> C-e M-w"))

(global-set-key (kbd "<M-up>") 'move-lines-up)
(global-set-key (kbd "<M-down>") 'move-lines-down)

(global-set-key (kbd "<C-return>") 'cua-rectangle-mark-mode)

(global-set-key (kbd "C-v") 'yank)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(delete-selection-mode 1)

(if (boundp '*my-backup-dir*)
    (let ((dir *my-backup-dir*))
      (setq backup-directory-alist
            `((".*" . , dir)))
      (setq auto-save-file-name-transforms
            `((".*" , dir t))))
  (message "Don't forget to specify a backup directory!"))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun ttl ()
  (interactive)
  (toggle-truncate-lines))

(defun vlm ()
  (interactive)
  (visual-line-mode))

(server-start)
