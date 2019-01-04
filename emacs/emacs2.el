(require 'cl)

(setq-default truncate-lines t)

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq-default tab-width 4
              indent-tabs-mode nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode -1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(delete-selection-mode t)

(savehist-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; Still blasts Eldoc message :(
(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)
(defun save-all ()
  (interactive)
  (save-some-buffers t))
;; (global-set-key (kbd "C-x C-s") 'save-all)

(setq-default sentence-end-double-space nil)

(let ((f "%* %b"))
  (setq-default mode-line-format
		`((:eval (if (get-buffer-process (current-buffer))
			     '(:propertize ,f face success)
			   ,f)))))

(global-set-key (kbd "C-z") 'undo)

(require 'dired)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(define-key dired-mode-map (kbd "<backspace>") #'dired-up-directory)
(setq dired-recursive-deletes 'always
      dired-auto-revert-buffer t)

(defun kill-this-buffer ()
  "Kill the current buffer. Ask no questions."
  (interactive)
  (kill-buffer))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; (setf ansi-color-for-comint-mode 'filter)
(setf ansi-color-for-comint-mode t)

(define-derived-mode wrapping-text-mode fundamental-mode
  (toggle-truncate-lines -1)
  (visual-line-mode t))
(add-to-list 'auto-mode-alist '("\\.tx\\'" . wrapping-text-mode))

(defun call-maybe-with-region (fun)
  "Build a function which calls FUN interactively, inserting the
region into minibuffer if it is active."
  (lexical-let ((fun fun))
    (lambda ()
      (interactive)
      (let ((init (if (region-active-p)
		              (buffer-substring-no-properties (point) (mark))
		            nil)))
	    (minibuffer-with-setup-hook (lambda () (when init (insert init)))
	      (call-interactively fun))))))

(defun google (term)
  (interactive "M")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))
(global-set-key (kbd "C-c g") (call-maybe-with-region 'google))

(global-set-key (kbd "C-c f c") #'make-frame)
(global-set-key (kbd "M-g") #'goto-line)
(setq mouse-autoselect-window 0.1)

(global-set-key (kbd "<f1>") #'buffer-menu)
(global-set-key (kbd "<mouse-5>") #'buffer-menu)
(global-set-key (kbd "<escape>") #'other-window)
(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-vertically)
(global-set-key (kbd "C-3") #'split-window-horizontally)
(windmove-default-keybindings)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(define-minor-mode clean-trailing-whitespace-mode
  "Delete trailing whitespace on save."
  :lighter " ws"
  (if clean-trailing-whitespace-mode
      (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace t)))
(add-hook 'groovy-mode-hook 'clean-trailing-whitespace-mode)
