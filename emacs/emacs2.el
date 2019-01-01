(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq tab-width 4
      indent-tabs-mode nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode -1)
(blink-cursor-mode -1)
(show-paren-mode 1)

;; Still blasts Eldoc message :(
;; (setq auto-save-visited-interval 1)
;; (auto-save-visited-mode 1)
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(global-set-key (kbd "C-x C-s") 'save-all)

(setq-default sentence-end-double-space nil)
(setq-default mode-line-format '("%* %b"))

(global-set-key (kbd "C-z") 'undo)

(require 'dired)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(define-key dired-mode-map (kbd "<backspace>") #'dired-up-directory)
(setq dired-recursive-deletes 'always)

(defun kill-this-buffer ()
  "Kill the current buffer. Ask no questions."
  (interactive)
  (kill-buffer))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(setf ansi-color-for-comint-mode 'filter)

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
