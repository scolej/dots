(defun define-keys (keymap &rest keys)
  "Make multiple bindings in a map."
  (cl-loop for (key binding) on keys by #'cddr do
           (define-key keymap (kbd key) binding)))

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; remember minibuffer history between restarts
(savehist-mode 1)
(repeat-mode 1)
(delete-selection-mode 1)

(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)

(global-auto-revert-mode 1)

(setq
 backup-directory-alist '((".*" . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 6
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t))
 auto-save-timeout 30
 auto-save-interval 0
 create-lockfiles nil)

(global-set-key (kbd "C-z") 'undo)

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(setq-default
 auto-hscroll-mode t
 hscroll-step 10
 hscroll-margin 2)

(setq save-silently t)

;; ruby stuff
(setq ruby-indent-level 4)

(defun beginning-of-line-toggle ()
  (interactive)
  (if (eq (- (point) (pos-bol)) (current-indentation))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key (kbd "<home>") 'beginning-of-line-toggle)
(global-set-key (kbd "<end>") 'end-of-line)

;; tab bar
(global-set-key (kbd "s-t") 'tab-new)
(global-set-key (kbd "s-w") 'tab-close)
(global-set-key (kbd "C-<next>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-S-<next>") 'tab-bar-move-tab)
(global-set-key (kbd "C-S-<prior>") 'tab-bar-move-tab-backward)

(fset 'yes-or-no-p 'y-or-n-p)

;; dired
(require 'dired)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(define-keys
 dired-mode-map
 "<DEL>" 'dired-jump
 "<mouse-2>" 'dired-find-file)
(setq dired-auto-revert-buffer t)

(setq completion-auto-help 'always
      completion-auto-select 'second-tab)

;; clojure
(require 'cider)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(define-key paredit-mode-map (kbd "C-<backspace>") 'backward-kill-sexp) ;; this one seems to be missing and results in broken syntax

(defun insert-lambda ()
  (interactive)
  (insert "(λ ())")
  (forward-char -2))
(define-key paredit-mode-map (kbd "C-\\") 'insert-lambda)

(global-set-key (kbd "<escape>") 'keyboard-quit)

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(setq save-interprogram-paste-before-kill t)

;; (global-tab-line-mode 1)
;; (global-set-key (kbd "<tab-line> <wheel-up>") 'tab-line-switch-to-prev-tab)
;; (global-set-key (kbd "<tab-line> <wheel-down>") 'tab-line-switch-to-next-tab)
;; (global-set-key (kbd "s-<left>") 'tab-line-switch-to-prev-tab)
;; (global-set-key (kbd "s-<right>") 'tab-line-switch-to-next-tab)

(global-set-key (kbd "C-<tab>") 'other-window)

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-w") 'bury-buffer)
(global-set-key (kbd "s-/") 'comment-dwim)

(setq-default truncate-lines t)

(setq-default
 mode-line-format
 '((:eval (cond
           ((get-buffer-process (current-buffer))
            '(:propertize ">>>" face (:background "orange")))
           ((and (buffer-file-name) (buffer-modified-p))
            '(:propertize "+" face (:background "yellow")))))
   (:eval (or (buffer-file-name) (buffer-name)))
   ":%l:%c"))

;; ergonomic copy cut paste
(global-set-key (kbd "<S-return>") 'yank)
(global-set-key (kbd "<S-backspace>") 'kill-region)
(defun maybe-copy-region ()
  (interactive)
  (if (region-active-p)
      (yank))     )

;;
;; Selected
;;

(defun clone-region (beg end)
  (interactive "r")
  (let ((deactivate-mark nil)
        (str (buffer-substring-no-properties beg end)))
    (save-excursion
      (goto-char end)
      (insert str))))

(defun surround-region (before &optional after)
  (interactive "M")
  (save-excursion
    (goto-char (min (point) (mark)))
    (insert before))
  (save-excursion
    (goto-char (max (point) (mark)))
    (insert (or after before))))

;;
;; Query replace using region
;;

(define-keys query-replace-map "p" 'backup)

(defvar-local query-replace-previous nil
  "The last query replace we did. A pair, the first element is the string to find,
the second element is the replacement.")

(defun query-replace-maybe-region ()
  "If there's a selection, prompt for replacement text for the
selection, otherwise call query-replace-regexp as normal."
  (interactive)
  (if (region-active-p)
      (let ((str (buffer-substring-no-properties (point) (mark))))
        (deactivate-mark)
        (goto-char (min (point) (mark)))
        (let ((rep (read-from-minibuffer
                    (format "Replace %s with: " str)
                    nil nil nil nil str))
              (str-esc (regexp-quote str)))
          (setq query-replace-previous (cons str-esc rep))
          (query-replace-regexp str-esc rep)))
    (call-interactively 'query-replace-regexp)))

(defun query-replace-resume ()
  "Resume a previous session of replacing."
  (interactive)
  (unless query-replace-previous (error "no previous replacement"))
  (query-replace-regexp
   (car query-replace-previous)
   (cdr query-replace-previous)))

(require 'selected)

(define-keys
 selected-keymap
 "<return>" 'kill-ring-save
 "r" 'query-replace-maybe-region
 ";" 'comment-dwim
 "c" 'clone-region
 "x" 'exchange-point-and-mark
 "\"" (lambda () (interactive (surround-region "\"")))
 "{" (lambda () (interactive (surround-region "{" "}")))
 "<" (lambda () (interactive (surround-region "<" ">")))
 "'" (lambda () (interactive (surround-region "'")))
 "`" (lambda () (interactive (surround-region "`")))
 "(" (lambda () (interactive (surround-region "(" ")")))
 "[" (lambda () (interactive (surround-region "[" "]")))
 "n" 'forward-search-region
 "p" 'backward-search-region
 "<escape>" 'keyboard-quit
 "m" 'copy-crumb
 "r" 'query-replace-maybe-region)

(selected-global-mode)

;;

(defun yank-para ()
  (interactive)
  (let ((b (save-excursion (backward-sentence) (point)))
        (e (save-excursion (forward-sentence) (point))))
    (kill-ring-save b e)))

;;

(defun dedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) t)
  (message "this window is now dedicated"))

(defun undedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (message "this window is no longer dedicated"))

;;

(use-package compilation-guile)

;;

;; (use-package tab-line
;;   :config
;;   (global-tab-line-mode 1)
;;   (setq tab-line-close-button nil))

(global-set-key (kbd "s-w") 'bury-buffer)

;;

(mapc
 (lambda (s) (put s 'disabled nil))
 '(narrow-to-page
   erase-buffer
   scroll-right
   scroll-left))

;; (setq mouse-wheel-tilt-scroll nil
;;       mouse-wheel-flip-direction t
;;       mouse-wheel-scroll-amount-horizontal 5)

;; (setq mouse-wheel-scroll-amount
;;       '(1 ((shfit) . ())))

;;

(defun pp-json (beg end)
  (interactive "r")
  (unless (region-active-p) (mark-sexp))
  (shell-command-on-region beg end "jq" nil t))

;;

(require 'hi-lock)

(defun toggle-highlight-symbol-at-point ()
  (interactive)
  (let ((regexp (find-tag-default-as-symbol-regexp)))
    (when regexp
      (if (assoc regexp hi-lock-interactive-lighters)
          (unhighlight-regexp regexp)
        (highlight-regexp regexp)))))

(global-set-key (kbd "<kp-decimal>") 'toggle-highlight-symbol-at-point)

;;

(use-package notes)

;;
;; Copying paths
;;

(defun copy-path-git ()
  "Copy the git-relative path to the current file."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (error "not in a git repo")))
         (abs (or (buffer-file-name)
                  (default-directory)))
         (str (file-relative-name abs root)))
    (kill-new str)
    (let ((x-select-enable-primary t))
      (x-select-text str))
    (message (format "Copied: %s" str))))

;; TODO there's some factoring here for sure
;; TODO if there's an active region, use the whole region and correctly indent the snippet
(defun copy-crumb ()
  "Copy file path, line number, and trimmed line."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (error "not in a git repo")))
         (abs (or (buffer-file-name)
                  (default-directory)))
         (linum (number-to-string (line-number-at-pos (point))))
         (prefix (concat (file-relative-name abs root) ":" linum))
         (body (if (region-active-p)
                   (concat "\n" (buffer-substring-no-properties (point) (mark)) "\n")
                 (concat " " (s-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol))) "\n")))
         (copy-str (concat prefix body)))
    (kill-new copy-str)
    (let ((x-select-enable-primary t))
      (x-select-text copy-str))
    ;; todo crumbs with %s in them !?
    (message (format "Copied crumb."))))

(defun copy-git-buffer-path ()
  "Copy the Git-root-relative path to the current buffer's file."
  (interactive)
  (let* ((git-root (or (locate-dominating-file default-directory ".git") (error "not in a git repo")))
         (full-path (or (buffer-file-name) default-directory))
         (str (file-relative-name full-path git-root)))
    (kill-new str)
    (message (format "Copied: %s" str))))

;; todo should these be git-aware?
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

