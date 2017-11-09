(setq-default c-basic-offset 4
              cursor-type '(bar . 2)
              dired-listing-switches "-alh"
              dired-auto-revert-buffer t
              dired-dwim-target t
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
              mode-line-format "%b %* %l"
              mouse-autoselect-window t
              mouse-wheel-progressive-speed nil
              mouse-wheel-scroll-amount '(4 ((shift) . 4))
              recentf-max-saved-items 100
              revert-without-query '(".*")
              ring-bell-function 'ignore
              save-interprogram-paste-before-kill t
              sentence-end-double-space nil
              set-mark-command-repeat-pop t
              scroll-conservatively 9999
              scroll-margin 0
              show-help-function nil
              show-paren-style 'parenthesis
              show-trailing-whitespace nil
              tab-width 4
              truncate-lines t
              truncate-partial-width-windows nil
              visible-bell nil)

;; Stop making vertical splits.
(setq-default split-width-threshold nil)

;; Backup set up.
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

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
(cua-mode 0)

(add-hook 'find-file-hook
          (lambda ()
            (let ((ext (file-name-extension buffer-file-name)))
              (when (or (string= ext "log")
                        (string= ext "logga"))
                (read-only-mode t)))))

(defun ttl ()
  "Shortcut for truncating lines."
  (interactive)
  (toggle-truncate-lines))

(defun words-dammit ()
  "I just want word wrapping!"
  (interactive)
  (fundamental-mode)
  (toggle-truncate-lines 0)
  (visual-line-mode t))

(defun copy-buffer-path ()
  "Copy the full path to the current buffer's file."
  (interactive)
  (let ((s (buffer-file-name)))
    (kill-new s)
    (message (format "Copied %s" s))))

(defun copy-buffer-path-and-line ()
  "Copy the full path to the current buffer's file and append a
colon followed by the line number."
  (interactive)
  (let ((s (concat (buffer-file-name)
                    ":"
                    (number-to-string (line-number-at-pos (point))))))
    (kill-new s)
    (message (format "Copied %s" s))))

(add-hook 'occur-hook #'occur-rename-buffer)
(add-hook 'occur-hook #'hl-line-mode)
(add-hook 'next-error-hook #'recenter)

(defun chunky-scroll-left () (interactive) (scroll-left 20))
(defun chunky-scroll-right () (interactive) (scroll-right 20))

(defun delete-other-frames ()
  (interactive)
  (mapc #'(lambda (f)
            (unless (eq f (selected-frame))
              (delete-frame f)))
        (frame-list)))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-c f c") 'make-frame)
(global-set-key (kbd "C-c f d") 'delete-frame)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-s s") 'sort-lines)
(global-set-key (kbd "M-s u") 'upcase-region)
(global-set-key [S-wheel-up] 'chunky-scroll-right)
(global-set-key [S-wheel-down] 'chunky-scroll-left)
(global-set-key (kbd "<S-prior>") 'chunky-scroll-right)
(global-set-key (kbd "<S-next>") 'chunky-scroll-left)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "C-c b l") 'copy-buffer-path-and-line)
(global-set-key (kbd "C-c b b") 'copy-buffer-path)
(global-set-key (kbd "<escape>") 'ibuffer)
(global-set-key (kbd "<S-return>") (lambda () (interactive) (insert (gui-get-primary-selection))))

;; Unmap shenanigans.
(global-set-key (kbd "<f2>") nil)
(global-set-key (kbd "C-h h") nil)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "M-S-y") 'yank-pop-forwards)

(use-package mega-highlight)

(use-package select-whole-lines
  :config
  (select-whole-lines-mode))

(use-package save-all-the-things
  :config
  (save-all-the-things-mode))

(use-package dired
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  ;; (remove-hook 'dired-mode-hook #'recenter)
  :bind (:map dired-mode-map
              ("<backspace>" . dired-up-directory)
              ("C-t" . nil)))

(use-package dired-x
  :bind (("M-j" . dired-jump)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)))

(use-package transpose-frame
  :bind (("C-x t" . transpose-frame)))

(use-package auto-complete
  :disabled
  :demand
  :config
  (ac-config-default)
  (setq ac-auto-show-menu t
        ac-use-quick-help nil)
  :bind (:map ac-completing-map
              ("<down>" . nil)
              ("<up>" . nil)
              ("<RET>" . nil)
              ("<return>" . nil)
              ("<tab>" . nil)
              ("<backtab>" . nil)
              ("C-p" . ac-previous)
              ("C-n" . ac-next)
              ("C-j" . ac-complete)
              ("<escape>" . keyboard-quit)))

(use-package multiple-cursors
  :config
  (add-to-list 'mc/cmds-to-run-once 'forward-whitespace)
  :bind (("C-S-n" . mc/mark-next-like-this)
         ("<M-S-down>" . mc/mark-next-lines)
         ("<M-S-up>" . mc/mark-previous-lines)))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package haskell-mode)

(use-package hindent
  :disabled
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package bm
  :bind (("<M-SPC>" . bm-toggle)))

(use-package markdown-mode)

(use-package ivy
  :demand
  :config
  (ivy-mode)
  (setq-default ivy-use-virtual-buffers t)
  :bind (("<escape>" . ivy-switch-buffer)
         :map ivy-switch-buffer-map
         ("<escape>" . minibuffer-keyboard-quit)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-S-s" . isearch-forward)))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy
        projectile-indexing-method 'alien
        projectile-switch-project-action 'projectile-commander))

(use-package counsel
  :bind (("C-c j" . counsel-git-grep)))

(use-package rust-mode :disabled)

(use-package drag-stuff
  :bind (("<M-up>" . drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

(use-package duplicate-thing
  :init
  (defun duplicate-thing-down ()
    (interactive)
    (duplicate-thing 1)
    (next-line))
  :bind (("<C-M-up>" . duplicate-thing)
         ("<C-M-down>" . duplicate-thing-down)))

(use-package org-mode
  :config
  (setq org-support-shift-select t)
  :bind (:map orgtbl-mode-map
              ("<backspace>" . nil)
              ("<DEL>" . nil)))

(use-package magit
  :config
  (setq magit-commit-show-diff nil)
  ;; Protect against accidental pushes to upstream
  (defadvice magit-push-current-to-upstream
      (around my-protect-accidental-magit-push-current-to-upstream)
    (let ((my-magit-ask-before-push t))
      ad-do-it))
  (defadvice magit-git-push (around my-protect-accidental-magit-git-push)
    (if (bound-and-true-p my-magit-ask-before-push)
        ;; Arglist is (BRANCH TARGET ARGS)
        (if (yes-or-no-p (format "Push %s branch upstream to %s? "
                                 (ad-get-arg 0) (ad-get-arg 1)))
            ad-do-it
          (error "Push to upstream aborted by user"))
      ad-do-it))
  (ad-activate 'magit-push-current-to-upstream)
  (ad-activate 'magit-git-push)
  :bind (("C-c m" . magit-status)))

(use-package feature-mode
  :bind (:map feature-mode-map
              ("C-c g" . jump-to-step-definition-current-line)))

(use-package ag
  :config
  (defun ag-here (str)
    (interactive "M")
    (ag str default-directory))
  (global-set-key (kbd "C-x a") #'ag-here))

(use-package geiser
  :config
  (add-hook 'geiser-mode-hook 'paredit-mode)
  (setq-default geiser-scheme-implementation 'chicken))

;; PIKA WIP

(defun insert-current-hhmm ()
  (interactive)
  (insert (format-time-string "%H%M" (current-time))))

(defun insert-current-date ()
  (interactive)
(insert (format-time-string "%Y-%m-%d" (current-time))))
(global-set-key (kbd "C-c t") #'insert-current-date)

(defvar pikatock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'insert-current-hhmm)
    (define-key map (kbd "C-c t") 'insert-current-date)
    map))

(defun pika-indent-function ()
  (save-excursion
    (indent-line-to
    (let ((c (char-after (+ (current-indentation) (point-at-bol)))))
      (cond
       ((null c) 0)
       ((char-equal ?- c) 4)
       (0))))))

(define-derived-mode pikatock-mode
  text-mode "Pikatock" "Major mode for time logs."
  (setq-local electric-indent-mode t)
  (setq-local indent-line-function #'pika-indent-function)
  (setq-local require-final-newline t))

(add-to-list 'auto-mode-alist '("\\.time\\'" . pikatock-mode))

;;

;; Don't lose text scaling when a buffer is reverted. Surely this should be default behaviour! :O
(setq text-scale-mode-amount 0)
(add-hook 'before-revert-hook (lambda () (setq text-scale-mode-amount-stashed text-scale-mode-amount)))
(add-hook 'after-revert-hook (lambda () (text-scale-increase text-scale-mode-amount-stashed)))

;;

(defun process-in-dir (dir program &rest args)
  "Start a process in a directory. Automatically create a buffer
for output. Spaces in the program will be split out into
arguments and joined with ARGS. ARGS are not split on spaces."
  (let* ((args1 (split-string program " "))
         (program (car args1))
         (args2 (append (cdr args1) args))
         (buf (get-buffer-create program)))
    (with-current-buffer (switch-to-buffer-other-window buf)
      (let ((default-directory dir))
        (read-only-mode -1)
        (erase-buffer)
        (apply #'start-process program buf program args2)
        (view-mode))))
  nil)
