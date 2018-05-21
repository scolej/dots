(setq-default c-basic-offset 4
              cursor-type 'box
              dired-listing-switches "-alh"
              dired-auto-revert-buffer t
              dired-dwim-target nil
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
              mode-line-format '("%* %b:%l %f")
              mouse-autoselect-window 1
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
              visible-bell nil
              frame-title-format '("%b"))

(setq x-pointer-shape x-pointer-top-left-arrow)

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
(tooltip-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode t)
(transient-mark-mode t)
(recentf-mode t)
(savehist-mode t)
(delete-selection-mode t)
(global-hl-line-mode 0)
(cua-mode t)

(add-hook 'find-file-hook
          (lambda ()
            (let ((ext (file-name-extension buffer-file-name)))
              (when (or (string= ext "log")
                        (string= ext "logga"))
                (read-only-mode t)
                (text-scale-set -1)
                (hl-line-mode)))))

(defun save-all ()
  "Save every buffer."
  (interactive)
  (save-some-buffers 'no-confirm))

(defun words-dammit ()
  "I just want word wrapping!"
  (interactive)
  (fundamental-mode)
  (toggle-truncate-lines 0)
  (visual-line-mode t))

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

(add-hook 'occur-hook #'occur-rename-buffer)
(add-hook 'occur-hook #'hl-line-mode)
(add-hook 'next-error-hook #'recenter) ;; TODO Does this actually work?
(add-hook 'archive-mode-hook #'hl-line-mode)

(defun chunky-scroll-left () (interactive) (scroll-left 20))
(defun chunky-scroll-right () (interactive) (scroll-right 20))
(defun small-scroll-left () (interactive) (scroll-left 10))
(defun small-scroll-right () (interactive) (scroll-right 10))

(defun delete-other-frames ()
  (interactive)
  (mapc #'(lambda (f)
            (unless (eq f (selected-frame))
              (delete-frame f)))
        (frame-list)))

(global-set-key (kbd "<C-SPC>") #'company-complete)
(global-set-key (kbd "<S-next>") #'chunky-scroll-left)
(global-set-key (kbd "<S-prior>") #'chunky-scroll-right)
(global-set-key (kbd "<escape>") #'ibuffer)
(global-set-key (kbd "<f5>") #'revert-buffer)
(global-set-key (kbd "<wheel-left>") #'small-scroll-right)
(global-set-key (kbd "<wheel-right>") #'small-scroll-left)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-c b b") #'copy-buffer-path)
(global-set-key (kbd "C-c b l") #'copy-buffer-path-and-line)
(global-set-key (kbd "C-c f c") #'make-frame)
(global-set-key (kbd "C-c f d") #'delete-frame)
(global-set-key (kbd "C-c l") #'list-processes)
(global-set-key (kbd "C-c r") #'revert-buffer)
(global-set-key (kbd "C-d") #'kill-whole-line)
(global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-x <down>") #'windmove-down)
(global-set-key (kbd "C-x <left>") #'windmove-left)
(global-set-key (kbd "C-x <right>") #'windmove-right)
(global-set-key (kbd "C-x <up>") #'windmove-up)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "M-s d") #'delete-trailing-whitespace)
(global-set-key (kbd "M-s s") #'sort-lines)
(global-set-key (kbd "M-s u") #'upcase-region)
(global-set-key [S-wheel-down] #'chunky-scroll-left)
(global-set-key [S-wheel-up] #'chunky-scroll-right)
(global-set-key (kbd "C-\\") #'replace-string)

;; Unmap shenanigans.
(global-set-key (kbd "<f2>") nil)
(global-set-key (kbd "C-h h") nil)

(defun help-at-point ()
  "Try to guess what the thing at point is and show help.
Surely this exists elsewhere."
  (interactive)
  (let ((x (intern (substring-no-properties (thing-at-point 'symbol)))))
    (cond ((functionp x) (describe-function x)   )
          ((describe-variable x)))))
(global-set-key (kbd "<f1>") #'help-at-point)
(global-set-key (kbd "C-h h") #'help-at-point)

(defun close-window-or-frame ()
  (interactive)
  (if (one-window-p) (delete-frame) (delete-window)))
(global-set-key (kbd "<f19>") #'close-window-or-frame)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "M-S-y") 'yank-pop-forwards)

(use-package mega-highlight)

(use-package save-all-the-things
  :config
  (add-hook 'find-file-hook #'save-all-the-things-mode))

(use-package co-man-der
  :config
  (add-hook 'co-man-der-mode-hook #'hl-line-mode))

(use-package eshell
  :init
  ;; Don't show status because it fights my simple modeline.
  (setq eshell-status-in-mode-line nil
        eshell-banner-message "")
  :config
  ;; Nasty: Eshell does it's own weird thing with keymaps, have to use a hook to configure.
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<up>") nil)
              (define-key eshell-mode-map (kbd "<down>") nil))))

(use-package dired
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  :bind (:map dired-mode-map
              ("<backspace>" . dired-up-directory)
              ("C-t" . nil)))

(use-package dired-x
  :bind (("M-j" . dired-jump)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)))

(use-package transpose-frame
  :bind (("C-x t" . transpose-frame)))

(use-package multiple-cursors
  :config
  (add-to-list 'mc/cmds-to-run-once 'forward-whitespace)
  :bind (("C-S-n" . mc/mark-next-like-this)
         ("<M-S-down>" . mc/mark-next-lines)
         ("<M-S-up>" . mc/mark-previous-lines)))

(use-package flycheck)

(use-package haskell-mode)

(defun google (term)
  (interactive "M")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))
(global-set-key (kbd "C-c g") #'google)

(defun hackage (term)
  (interactive "M")
  (browse-url
   (concat "https://hackage.haskell.org/packages/search?terms="
           (url-encode-url term))))

(use-package hindent
  :disabled
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package bm
  :bind (("<M-SPC>" . bm-toggle)
         ("M-." . bm-next)
         ("M-," . bm-previous)))

(use-package markdown-mode)

(use-package ivy
  :demand
  :config
  (ivy-mode)
  (setq-default ivy-use-virtual-buffers nil
                ivy-do-completion-in-region nil
                ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :bind (("<escape>" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         :map ivy-switch-buffer-map
         ("<escape>" . minibuffer-keyboard-quit)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-S-s" . isearch-forward)))

(use-package counsel
  :config
  (counsel-mode))

(use-package company
  :config
  (global-company-mode t))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy
        projectile-indexing-method 'alien
        projectile-switch-project-action 'projectile-find-file)
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "bin")
  :bind (("<S-escape>" . #'projectile-find-file)))

(use-package counsel
  :bind (("C-c j" . counsel-git-grep)))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode))

(use-package racer
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

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

(use-package org
  :config
  (setq org-support-shift-select t)
  :bind (:map org-mode-map
              ("<S-left>" . nil)
              ("<S-right>" . nil)))

(use-package org-table
  :bind (:map orgtbl-mode-map
              ("<backspace>" . nil)
              ("<DEL>" . nil)
              ("<tab>" . orgtbl-tab)
              ;; ("TAB" . nil)
              ))

(use-package magit
  :config
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
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind (("C-c m" . magit-status)))

(use-package feature-mode
  :bind (:map feature-mode-map
              ("C-c g" . jump-to-step-definition-current-line)
              ("M-." . nil)
              ("M-," . nil)))

(use-package ag
  :config
  (defun ag-here (str)
    (interactive "M")
    (ag str default-directory))
  (global-set-key (kbd "C-x a") #'ag-here))

(use-package back-button
  :config
  (back-button-mode t)
  :bind (("<M-right>" . #'back-button-local-forward)
         ("<M-left>" . #'back-button-local-backward)))

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
  (let ((before-column (- (point) (point-at-bol) (current-indentation))) ;; Current column, relative to indentation
        (c (char-after (+ (current-indentation) (point-at-bol))))) ;; First character on line
    (indent-line-to
     (cond ((and (not (null c)) (char-equal ?- c)) 4)
           ((= (point) (point-at-bol)) 4)
           (0)))
    (set-window-point nil
                      (max
                       (+ (point-at-bol) (current-indentation) before-column) ;; Restore previous position which was after indent
                       (+ (point-at-bol) (current-indentation)))))) ;; Move point to first character on line

(defvar pikatock-highlights '(
                              ("^....-..-..*$" . font-lock-function-name-face)
                              ("^....-...." . font-lock-variable-name-face)
                              (":" . font-lock-comment-face)
                              ))

(define-derived-mode pikatock-mode
  text-mode "Pikatock" "Major mode for time logs."
  (setq-local indent-line-function #'pika-indent-function)
  (setq-local require-final-newline t)
  (setq-local font-lock-defaults '(pikatock-highlights)))

(add-to-list 'auto-mode-alist '("\\.time\\'" . pikatock-mode))
(add-to-list 'auto-mode-alist '("\\.moss\\'" . co-man-der-mode))

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

;;

(defun duplicate-buffer ()
  (interactive)
  (let ((b (get-buffer-create (generate-new-buffer-name (buffer-name)))))
    (copy-to-buffer b (point-min) (point-max))
    (switch-to-buffer b)))

(defun gitted-p ()
  "Is this file in a git repo?"
  (null (locate-dominating-file default-directory ".git")))

(defun nxml-narrow ()
  "Narrow the buffer the the XML element at point."
  (interactive)
  (let ((p0 (point-at-bol))
        (p1 (save-excursion
              (nxml-forward-element)
              (point))))
    (narrow-to-region p0 p1)))

(defun xml-wrap-selection-with-tag (tag)
  (interactive "M")
  (when (region-active-p)
    (insert "</" tag ">")
    (save-excursion
      (goto-char (mark))
      (insert "<" tag ">"))
    (deactivate-mark)))

(defun dedicate-window ()
  (interactive)
  (set-window-dedicated-p nil t))

(defun undedicate-window ()
  (interactive)
  (set-window-dedicated-p nil nil))

(defun tail-tail-tail ()
  (interactive)
  (revert-buffer)
  (end-of-buffer))
(global-set-key (kbd "<C-next>") 'tail-tail-tail)

(defun purge-trailing-whitespace ()
  (interactive)
  (mapc
   (lambda (f)
     (with-temp-file f
       (insert-file-contents f)
       (delete-trailing-whitespace)))
   (find-lisp-find-files default-directory ".*")))

;; Common theme things
(set-face-attribute 'mode-line nil
                    :box '(:line-width 2 :style released-button)))
(set-face-attribute 'mode-line-inactive nil
                    :box '(:line-width 2 :style released-button)))
(set-face-attribute 'cursor nil :background "red" :foreground "white")
