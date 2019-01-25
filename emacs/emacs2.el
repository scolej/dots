(require 'cl)

(setq-default truncate-lines t)

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq-default tab-width 4
              indent-tabs-mode nil
              c-basic-offset 4)

(setq mouse-wheel-progressive-speed nil)

(setq revert-without-query '(".*"))

(setq hscroll-margin 15
      scroll-margin 0)

(setq set-mark-command-repeat-pop t)

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

(global-set-key (kbd "<f5>") 'revert-buffer)

(defun save-all ()
  (interactive)
  (let ((inhibit-message t))
    (save-some-buffers t)))
(global-set-key (kbd "<f12>") 'save-all)

(global-eldoc-mode -1)
(setq auto-save-visited-interval 1)
(auto-save-visited-mode 2)

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
(define-key dired-mode-map (kbd "<backspace>") #'dired-up-directory)
(setq dired-recursive-deletes 'always
      dired-auto-revert-buffer t
      dired-clean-confirm-killing-deleted-buffers nil)

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
(global-set-key (kbd "M-g") #'goto-line)
(setq mouse-autoselect-window 0.1)

(global-set-key (kbd "<mouse-5>") #'buffer-menu)
(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-vertically)
(global-set-key (kbd "C-3") #'split-window-horizontally)

(require 'dired-x)
(global-set-key (kbd "<escape>") #'dired-jump)

;; FIXME Need these?
(defun point-line-start () (save-excursion (beginning-of-line) (point)))
(defun point-line-end () (save-excursion (end-of-line) (point)))

(defun duplicate-dwim ()
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring-no-properties (point) (mark)))
            (deactivate-mark nil))
        (save-excursion
          (goto-char (max (point) (mark)))
          (insert text)))
    (let ((text (buffer-substring-no-properties (point-line-start)
                                                (point-line-end))))
      (save-excursion
        (end-of-line)
        (newline)
        (insert text)))))
(global-set-key (kbd "C-=") 'duplicate-dwim)

(setq backup-by-copying t
      backup-directory-alist '((".*" . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; (define-minor-mode clean-trailing-whitespace-mode
;;   "Delete trailing whitespace on save."
;;   :lighter " ws"
;;   (if clean-trailing-whitespace-mode
;;       (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
;;     (remove-hook 'before-save-hook 'delete-trailing-whitespace t)))
;; (add-hook 'groovy-mode-hook 'clean-trailing-whitespace-mode)
;; (add-hook 'emacs-lisp-mode-hook 'clean-trailing-whitespace-mode)
;; (add-hook 'wrapping-text-mode-hook 'clean-trailing-whitespace-mode)
;; (add-hook 'c-mode-hook 'clean-trailing-whitespace-mode)
;; (add-hook 'cucumber-mode-hook 'clean-trailing-whitespace-mode)

(require 'ibuffer)
(add-to-list 'ibuffer-formats '(mark modified " " name " " filename))
(defun ibuffer-maybe-visit-buffer (event)
  "Hacky conditional visit based on click location. Should
probably do a smarter check than if point is at end of line."
  (interactive "e")
  (let ((pos (posn-point (event-end event))))
    (goto-char pos)
    (unless (equal (point) (point-at-eol))
      (ibuffer-visit-buffer))))
(define-key ibuffer-name-map [mouse-1] 'ibuffer-maybe-visit-buffer)
(setq ibuffer-display-summary nil
      ibuffer-expert t)
(defun ibuffer-switcher ()
  (interactive)
  (ibuffer)
  (unless (equal ibuffer-sorting-mode 'recency)
    (ibuffer-do-sort-by-recency))
  (beginning-of-buffer))
(defadvice ibuffer-update-title-and-summary (after remove-column-titles nil activate)
  (with-current-buffer "*Ibuffer*"
    (read-only-mode -1)
    (goto-char 1)
    (search-forward "]\n" nil t)
    (delete-region 1 (point))
    (read-only-mode 1)))

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
(setq ag-reuse-window nil)

(require 'transpose-frame)
(global-set-key (kbd "C-x t") 'transpose-frame)

(defun set-default-directory (d)
  (interactive "D")
  (setq-local default-directory d))

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
  :bind (("C-s" . swiper-with-region)))

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
  (setq highlight-thing-what-thing nil
        highlight-thing-prefer-active-region t
        highlight-thing-exclude-thing-under-point t)
  (global-highlight-thing-mode t))

;; (use-package back-button
;;   :config
;;   (setq back-button-index-timeout nil)
;;   :bind (("<M-left>" . 'back-button-local-backward)
;;          ("<M-right>" . 'back-button-local-forward)))
