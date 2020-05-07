;;
;; Lots of random customizations.
;; Work in progress.
;;

;;
;; TODO & ideas
;;

;; Better jumping to file paths under point.
;; Tiny minor-mode?

;; Long mouse presses for word & line selection

;; C-x C-e for lisp should eval-region if region is active.

;;
;; Dired
;;

(require 'dired-x)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(defun dired-find-here (name)
  ;; TODO ? initial input "**" with cursor in the middle.
  (interactive "sFile name wildcard: ")
  (find-name-dired default-directory name))

(setq mouse-1-click-follows-link 450)
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-display-file)
(define-key dired-mode-map (kbd "o") 'dired-display-file)
(define-key dired-mode-map (kbd "i") 'dired-find-here)
(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)

;;
;; Occur
;;

(add-hook 'occur-hook 'occur-rename-buffer)

(define-key occur-mode-map (kbd "n")
  (lambda () (interactive)
    (occur-next)
    (occur-mode-display-occurrence)))

(define-key occur-mode-map (kbd "p")
  (lambda () (interactive)
    (occur-prev)
    (occur-mode-display-occurrence)))

;;
;; Misc
;;

(global-set-key (kbd "C-x C-d") nil)

;; Just use find-file?
(global-set-key (kbd "C-x d") nil)

(global-set-key (kbd "C-\\") 'replace-string)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; (global-set-key (kbd "<kp-5>") 'kill-whole-line)
;; (global-set-key (kbd "<kp-1>") 'kill-region)
;; (global-set-key (kbd "<kp-2>") 'maybe-copy-whole-line)
;; (global-set-key (kbd "<kp-3>") 'yank)

(setq-default
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %b:%l:%c"))

(setq-default show-trailing-whitespace t)

(setq split-width-threshold nil
      split-height-threshold nil
      yank-handled-properties nil
      disabled-command-function nil)

(global-set-key (kbd "<f1>") 'switch-to-buffer)

(defun new-line (n)
  (let* ((bol (point-at-bol))
         (indent (buffer-substring-no-properties bol (+ bol (current-indentation)))))
    (if (eq n 'up)
        (progn (goto-char bol)
               (save-excursion (insert "\n"))
               (insert indent))
      (progn (goto-char (point-at-eol))
             (insert "\n")
             (insert indent)))))

(defun new-line-above () (interactive) (new-line 'up))
(defun new-line-below () (interactive) (new-line nil))

(global-set-key (kbd "C-S-o") 'new-line-above)
(global-set-key (kbd "C-o") 'new-line-below)

(setq-default case-fold-search t)
(setq completion-ignore-case t)

(setq delete-selection-save-to-register "d")
(global-set-key (kbd "M-v") 'delete-selection-repeat-replace-region)

;;
;; Saving
;;

(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "<f12>") 'save-all)

;;
;; Windowing
;;

(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "C-`") 'make-frame)
(global-set-key (kbd "M-o") 'other-window)

;;
;; Googling
;;

(require 'cl)

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
  (interactive "MGoogle: ")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))

(global-set-key (kbd "C-c g") (call-maybe-with-region 'google))

;;
;; Query replace using region
;;

(defun query-replace-maybe-region ()
  (interactive)
  (if (region-active-p)
      (let ((str (buffer-substring-no-properties (point) (mark))))
        (deactivate-mark)
        (goto-char (min (point) (mark)))
        (query-replace-regexp
         str
         (read-from-minibuffer (format "Replace %s with: " str))))
    (call-interactively 'query-replace-regexp)))

(global-set-key (kbd "M-%") 'query-replace-maybe-region)

;;
;; Duplicating
;;

(defun duplicate-region (dir)
  (interactive)
  (let* ((p (point))
         (m (mark))
         (text (buffer-substring-no-properties p m))
         (deactivate-mark nil))
    (save-excursion
      (cond
       ((eq dir 'down) (goto-char (max p m)) (insert text))
       ((eq dir 'up) (goto-char (min p m)) (insert-before-markers text))))))

(defun duplicate-line (dir)
  (interactive)
  (let* ((bol (point-at-bol))
         (eol (point-at-eol))
         (pos-on-line (- (point) bol))
         (text (buffer-substring-no-properties bol eol)))
    (cond ((eq dir 'down) (forward-line))
          ((eq dir 'up) (beginning-of-line)))
    (save-excursion (insert text "\n"))
    (forward-char pos-on-line)))

(defun duplicate (dir)
  (interactive)
  (if (region-active-p)
      (duplicate-region dir)
    (duplicate-line dir)))

(defun duplicate-up () (interactive) (duplicate 'up))
(defun duplicate-down () (interactive) (duplicate 'down))

(global-set-key (kbd "<C-M-up>") 'duplicate-up)
(global-set-key (kbd "<C-M-down>") 'duplicate-down)

;;
;; Dragging
;;

(defun drag (dir)
  (interactive)
  (unless (region-active-p)
    (let* ((bol (point-at-bol))
           (eol (point-at-eol))
           (pos-on-line (- (point) bol))
           (text (buffer-substring-no-properties bol eol)))
      (delete-region bol (progn (forward-line) (point)))
      (cond ((eq dir 'down) (end-of-line) (newline))
            ((eq dir 'up) (forward-line -1) (save-excursion (newline))))
      (save-excursion (insert text))
      (forward-char pos-on-line))))

(defun drag-up () (interactive) (drag 'up))
(defun drag-down () (interactive) (drag 'down))

(global-set-key (kbd "<M-down>") 'drag-down)
(global-set-key (kbd "<M-up>") 'drag-up)

;;
;; Buffer killer!
;;

(defun really-kill-buffer ()
  (interactive) (kill-buffer nil))

(global-set-key (kbd "C-x k") 'really-kill-buffer)

;;
;;
;;

(defun isearch-use-region (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (when (use-region-p)
    (let ((search (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))))
      (setq deactivate-mark t)
      (isearch-yank-string search))))

(advice-add 'isearch-forward :after 'isearch-use-region)
(advice-add 'isearch-backward :after 'isearch-use-region)

(setq isearch-allow-scroll t)

;;
;;
;;

(load "idle.el")
(load "trails.el")
;; (load "delete.el")
(load "scratchy.el")

;;
;;
;;

(require 'compile)

(defun compilation-next-and-visit ()
    (interactive)
    (compilation-next-error 1)
    (compilation-display-error))

(defun compilation-prev-and-visit ()
  (interactive)
  (compilation-previous-error 1)
  (compilation-display-error))

(define-key compilation-mode-map (kbd "n") 'compilation-next-and-visit)
(define-key compilation-mode-map (kbd "p") 'compilation-prev-and-visit)

;;
;;
;;

(setq completion-styles '(basic partial-completion emacs22 substring initials))

;;
;;
;;

(defun copy-buffer-path ()
  "Copy the full path to the current buffer's file."
  (interactive)
  (let ((str (cond (buffer-file-name)
                   (default-directory))))
    (kill-new str)
    (message (format "Copied: %s" str))))

(defun copy-buffer-path-and-line ()
  "Copy the full path to the current buffer's file and append a
colon followed by the line number."
  (interactive)
  (let ((s (concat (buffer-file-name)
                   ":"
                   (number-to-string (line-number-at-pos (point))))))
    (kill-new s)
    (message (format "Copied: %s" s))))

;;
;;
;;

(global-set-key (kbd "<escape>") 'dired-jump)
;; (global-set-key (kbd "<escape>") 'execute-extended-command)

;;
;;
;;

(require 'ffap)

(defun ffap-git-diff-file (str)
  (substring str 2))
(add-to-list 'ffap-alist '("[ab]/.*" . ffap-git-diff-file))

;;
;;
;;

(defun insert-random-password ()
  (interactive)
  (insert (shell-command-to-string "openssl rand -base64 20")))

;;
;;
;;

(defun minibuffer-exit-insert ()
  "Inserts the minibuffer text at point in the buffer from which
minibuffer was started."
  (interactive)
  (let ((txt (minibuffer-contents-no-properties)))
    (with-selected-window (minibuffer-selected-window)
      (insert txt))
    (minibuffer-keyboard-quit)))

(define-key minibuffer-local-map (kbd "C-M-j") 'minibuffer-exit-insert)

;;
;;
;;

(add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))

(set-face-attribute 'mode-line nil
                    :height 75 :box nil :overline nil :underline nil
                    :foreground "#555555" :background "#eeeeee"
                    :inverse-video nil)
(set-face-attribute 'mode-line-inactive nil
                    :inherit 'mode-line
                    :foreground nil :background nil
                    :box nil :weight 'normal)

(set-face-attribute 'fringe nil
                    :inherit 'default
                    ;; For some reason must be explicit about bg colour :S
                    ;; :background nil
                    :background "#ffffff"
                    :foreground nil)
(set-face-attribute 'vertical-border nil
                    :inherit 'mode-line :inverse-video t)

;;
;;
;;

(defun find-next-file (&optional offset)
  "Find a file in order relative to the current file based on OFFSET."
  (interactive)
  (let* ((full-name (buffer-file-name))
         (f (file-name-nondirectory full-name))
         (d (file-name-directory full-name))
         (fs (directory-files d))
         (i (seq-position fs f))
         (next (seq-elt fs (+ i (or offset 1)))))
    (if (equal next "..") (dired d)
      (find-file next))))

(defun find-prev-file ()
  (interactive)
  (find-next-file -1))

(global-set-key (kbd "C-x <down>") 'find-next-file)
(global-set-key (kbd "C-x <up>") 'find-prev-file)

;;
;;
;;

(when (boundp 'terminal-prog)
  (defun term-here ()
    (interactive)
    (start-process "term" nil terminal-prog))
  (global-set-key (kbd "C-x t") 'term-here))

;;
;;
;;

(defun dedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) t))

;;
;;
;;

(defun quick-next ()
  (interactive)
  (when isearch-string
    (search-forward isearch-string)))

(defun quick-prev ()
  (interactive)
  (when isearch-string
    (search-backward isearch-string)))

(global-set-key (kbd "M-n") 'quick-next)
(global-set-key (kbd "M-p") 'quick-prev)

;;
;;
;;

(require 'scheme)
(require 'cmuscheme)

(defun scheme-load-this-file ()
  (interactive)
  (save-buffer)
  (scheme-load-file (buffer-file-name)))

(define-key scheme-mode-map (kbd "C-c C-l") 'scheme-load-this-file)
(define-key scheme-mode-map (kbd "<f11>") 'scheme-load-this-file)

(setq scheme-program-name "/usr/local/bin/guile")

;;
;;
;;

(defun common-comint-setup ()
  (setq show-trailing-whitespace nil
        truncate-lines nil))

(add-hook 'comint-mode-hook 'common-comint-setup)

;;

(define-derived-mode tab-mode view-mode "Tab"
  ;; (read-only-mode 1)
  ;; Doesn't work. trails clobbers it. How fix?
  ;; (setq show-trailing-whitespace nil)
  )

(add-to-list 'auto-mode-alist '("\\.tab\\'" . tab-mode))
