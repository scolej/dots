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

(defun load-font (f)
  (if (find-font (font-spec :name f))
      (progn (set-default-font f)
             (add-to-list 'default-frame-alist `(font . ,f)))
    nil))


;; Don't forget, for OSX:
;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 999
;; to turn of anti-aliasing.

(or
 (load-font "Menlo 11")
 (load-font "Consolas 11"))

(or
 (ignore-errors (load-theme 'white-sand) t)
 (ignore-errors (load-theme 'solarized-light) t))

(set-face-attribute 'cursor nil :background "#ff0000")

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

;; TODO fix this
(add-hook 'buffer-menu-mode-hook
          (lambda ()
            (hl-line-mode)
            ;; (set-window-dedicated-p (get-buffer-window) t)
            ))

(defun perm ()
  (interactive)
  (set-window-dedicated-p (get-buffer-window) t))

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

;; http://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  ;; (find-file (ffap-guesser))
  (if (not (equal line-num 0))
      (goto-line line-num)))

(global-set-key (kbd "C-c C-o") 'find-file-at-point-with-line)

(global-set-key (kbd "C-z") 'undo)

(when (require 'dired-details+ nil :noerror)
  (setf dired-details-propagate-flag t)
  (setf dired-details-hidden-string "")
  (add-hook 'dired-mode (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)))

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

;; Copy line. Not working very well at the moment.
(global-set-key (kbd "C-c C-c") (kbd "C-a <C-SPC> C-e M-w"))

(when (require 'drag-stuff nil :noerror)
  (drag-stuff-global-mode))

(global-set-key (kbd "<C-return>") 'cua-rectangle-mark-mode)
(global-set-key (kbd "C-v") 'yank)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(delete-selection-mode 1)

;; Stop polluting the entire filesystem with backup files
(if (boundp '*my-backup-dir*)
    (let ((dir *my-backup-dir*))
      (setq backup-directory-alist
            `((".*" . , dir)))
      (setq auto-save-file-name-transforms
            `((".*" , dir t))))
  (message "Don't forget to specify a backup directory!"))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TODO fix this
;; (add-hook 'dired-mode-hook (lambda () (set-window-dedicated-p (get-buffer-window) t)))

(set-default 'truncate-lines t)

(defun ttl ()
  (interactive)
  (toggle-truncate-lines))

(defun vlm ()
  (interactive)
  (visual-line-mode))

(server-start)
