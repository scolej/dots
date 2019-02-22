;; Idle help? After a delay, if help window is visible and thing at point has help, show it.

;; More convenient cut copy paste. <backspace> <enter>? <S-enter>?
;; https://github.com/Kungsgeten/selected.el

(require 'cl)

(setq-default truncate-lines t)

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq-default tab-width 4
              indent-tabs-mode nil
              c-basic-offset 4)

(setq mouse-wheel-progressive-speed nil)

(setq revert-without-query '(".*"))

(setq hscroll-margin 0
      scroll-margin 0
      split-width-threshold 150
      split-height-threshold 100)

(setq set-mark-command-repeat-pop t)

(setq save-interprogram-paste-before-kill t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode -1)
(blink-cursor-mode -1)

(show-paren-mode 1)
(delete-selection-mode 1)
(savehist-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

(global-set-key (kbd "<f5>") 'revert-buffer)

;; FIXME It would be nice if this messaged how many buffers it had saved.
(defun save-all ()
  (interactive)
  (let ((inhibit-message t))
    (save-some-buffers t)))
(global-set-key (kbd "<f12>") 'save-all)

(setq-default sentence-end-double-space nil)

;; FIXME Should probably do "..." if truncated.
;; FIXME Doesn't work if modeline has scaled text.
(defun file-fitting-window ()
  "Returns the current file name, truncated to fit the window width."
  (let ((n (buffer-file-name))
        (w (- (window-width) 3)))
    (if n (substring n (max 0 (- (length n) w)))
      (buffer-name))))

;; (setq-default
;;  mode-line-format
;;  `("%* " (:eval (file-fitting-window))))

(let ((f '("%* " (:eval (file-fitting-window)))))
  (setq-default
   mode-line-format
   `((:eval (if (get-buffer-process (current-buffer))
			    '(:propertize ,f face success)
			  '(,f))))))

(global-set-key (kbd "C-z") 'undo)

(defun copy-buffer-path ()
  "Copy the full path to the current buffer's file."
  (interactive)
  (let ((str (cond (buffer-file-name)
                   (default-directory))))
    (kill-new str)
    (message (format "Copied %s" str))))
(defun copy-buffer-path-and-line ()
  "Copy the full path to the current buffer's file and append a
colon followed by the line number."
  (interactive)
  (let ((s (concat (buffer-file-name)
                   ":"
                   (number-to-string (line-number-at-pos (point))))))
    (kill-new s)
    (message (format "Copied %s" s))))
(global-set-key (kbd "C-c b b") #'copy-buffer-path)
(global-set-key (kbd "C-c b l") #'copy-buffer-path-and-line)


(require 'dired)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'hl-line-mode)
(define-key dired-mode-map (kbd "<backspace>") #'dired-up-directory)
(define-key dired-mode-map (kbd "r") #'ag-here)
(setq dired-recursive-deletes 'always
      dired-auto-revert-buffer t
      dired-clean-confirm-killing-deleted-buffers nil)

(defun dired-display-end-of-file ()
  (interactive)
  (dired-display-file)
  (with-selected-window
   (other-window-for-scrolling)
   (end-of-buffer)))
(define-key dired-mode-map (kbd "C-O") #'dired-display-end-of-file)

(defun kill-this-buffer ()
  "Kill the current buffer. Ask no questions."
  (interactive)
  (kill-buffer))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

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
(global-set-key (kbd "C-c f d") #'delete-frame)
(global-set-key (kbd "M-g") #'goto-line)

(setq mouse-autoselect-window 0.1)

(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-vertically)
(global-set-key (kbd "C-3") #'split-window-horizontally)

(require 'dired-x)

(defun escapy ()
  (interactive)
  (if (active-minibuffer-window)
      (top-level)
    (dired-jump)))

(global-set-key (kbd "<escape>") #'escapy)

;; FIXME Need these?
(defun point-line-start () (save-excursion (beginning-of-line) (point)))
(defun point-line-end () (save-excursion (end-of-line) (point)))

;; FIXME Region should be deactivated, but consecutive invokations should still work.
(defun duplicate-dwim ()
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring-no-properties (point) (mark)))
            (deactivate-mark nil))
        (save-excursion
          (goto-char (max (point) (mark)))
          (insert text)))
    ;; FIXME buffer-substring-no-properties why so long? it's so common...
    (let ((text (buffer-substring-no-properties (point-line-start)
                                                (point-line-end))))
      (save-excursion
        (end-of-line)
        (newline)
        (insert text)))))

;; FIXME I think the Eclipse bindings are better.
(global-set-key (kbd "C-=") 'duplicate-dwim)

(setq backup-by-copying t
      backup-directory-alist '((".*" . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil)

(define-minor-mode clean-trailing-whitespace-mode
  "Delete trailing whitespace on save."
  :lighter " ws"
  (if clean-trailing-whitespace-mode
      (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace t)))
(add-hook 'groovy-mode-hook 'clean-trailing-whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'clean-trailing-whitespace-mode)
(add-hook 'wrapping-text-mode-hook 'clean-trailing-whitespace-mode)
(add-hook 'c-mode-hook 'clean-trailing-whitespace-mode)
(add-hook 'pikatock-mode-hook 'clean-trailing-whitespace-mode)
(add-hook 'cucumber-mode-hook 'clean-trailing-whitespace-mode)

(defun drag (direction)
  (interactive)
 (if (region-active-p)
      ()
    (let ((pos-on-line (- (point) (point-line-start)))
          (text (buffer-substring-no-properties (point-line-start) (1+ (point-line-end)))))
      (delete-region (point-line-start) (1+ (point-line-end)))
      (forward-line (if (equal direction 'up) -1 1))
      (save-excursion (insert text))
      (forward-char pos-on-line))))
(defun drag-down () (interactive) (drag 'down))
(defun drag-up () (interactive) (drag 'up))
(global-set-key (kbd "<M-down>") 'drag-down)
(global-set-key (kbd "<M-up>") 'drag-up)

(require 'ag)
(defun ag-here (str)
  (interactive "M")
  (ag str default-directory))
(global-set-key (kbd "C-c r") 'ag-here)
(define-key ag-mode-map (kbd "r") 'ag-here)

(require 'transpose-frame)
(global-set-key (kbd "C-x t") 'transpose-frame)

(require 'use-package)

(use-package swiper
  :config
  (defun swiper-with-region ()
    (interactive)
    ;; FIXME This is so common, make it a function!
    (let ((initial (if (region-active-p)
                       (buffer-substring-no-properties (point) (mark))
                     nil)))
      (deactivate-mark)
      (swiper initial)))
  :bind (("<f3>" . swiper-with-region)))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers nil
        ivy-do-completion-in-region nil
        ivy-use-selectable-prompt t)
  (ivy-mode 1)
  :bind (("<f1>" . 'ivy-switch-buffer)))

(use-package counsel
  :config
  (counsel-mode 1)
  :bind (:map counsel-mode-map
         ;; FIXME This doesn't work?
         ("M-y" . nil)))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-switch-project-action 'projectile-dired)
  (projectile-mode 1)
  :bind (("<f2>" . 'projectile-find-file)
         ("C-c p p" . 'projectile-switch-project)))

(use-package nxml-mode
  :config
  (setq nxml-child-indent 4
        nxml-attribute-indent 4))

(use-package highlight-thing
  :config
  ;; FIXME It would be nice not to highlight whitespace!
  (setq highlight-thing-what-thing nil
        highlight-thing-prefer-active-region t
        highlight-thing-exclude-thing-under-point t)
  (global-highlight-thing-mode t))
