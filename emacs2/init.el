(load-theme 'modus-operandi-deuteranopia)

;;
;; Handier binding
;;

(defun define-keys (keymap &rest keys)
  "Make multiple bindings in a map."
  (cl-loop for (key binding) on keys by #'cddr do
           (define-key keymap (kbd key) binding)))

(defun keymap (&rest bindings)
  "Make a new keymap with bindings. Return that map."
  (let ((map (make-sparse-keymap)))
    (apply 'define-keys map bindings)
    map))

;;

(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; remember minibuffer history between restarts
(savehist-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(setq-default truncate-lines t)

(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)

(global-auto-revert-mode 1)

(set-fringe-mode '(12 . 0))

(setq
 backup-directory-alist '((".*" . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 6
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t))
 auto-save-timeout 30
 auto-save-interval 0
 create-lockfiles nil)

(setq sentence-end-double-space nil)

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(setq-default
 auto-hscroll-mode t
 hscroll-step 10
 hscroll-margin 2
 mouse-wheel-scroll-amount-horizontal 10)

(setq mouse-wheel-scroll-amount '(4))

(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

(setq mouse-autoselect-window 0.1)

(setq-default save-silently t)

(setq save-interprogram-paste-before-kill t)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "s-o") 'find-file)
(global-set-key (kbd "s-r") 'isearch-backward)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-<delete>") 'kill-word)

(define-keys
 minibuffer-mode-map
 "<escape>" 'minibuffer-keyboard-quit
 "s-r" 'isearch-backward)

(setq-default cursor-in-non-selected-windows '(hbar . 5))

(set-face-attribute 'cursor nil :background "#ff0000")

(defun beginning-of-line-toggle ()
  (interactive)
  (when this-command-keys-shift-translated (set-mark (point)))
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
(global-set-key (kbd "s-<next>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-<prior>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-S-<next>") 'tab-bar-move-tab)
(global-set-key (kbd "C-S-<prior>") 'tab-bar-move-tab-backward)
(global-set-key (kbd "C-<wheel-down>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-<wheel-up>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-S-<wheel-right>") 'tab-bar-move-tab)
(global-set-key (kbd "C-S-<wheel-left>") 'tab-bar-move-tab-backward)

(defun tab-bar-name-first-window ()
  (truncate-string-to-width
   (buffer-name (window-buffer (frame-first-window)))
   tab-bar-tab-name-truncated-max nil nil "…"))

(setq
 tab-bar-tab-name-truncated-max 40
 tab-bar-close-button-show nil
 tab-bar-tab-name-function 'tab-bar-name-first-window
 tab-bar-auto-width nil)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<C-tab>") 'other-window)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-vertically)
(global-set-key (kbd "s-3") 'split-window-horizontally)
(global-set-key (kbd "s-0") 'delete-window)

(global-set-key (kbd "<s-left>") 'previous-buffer)
(global-set-key (kbd "<s-right>") 'next-buffer)

(global-set-key (kbd "<s-right>") 'next-buffer)




;;

(load "grep2")

;;
;; dired
;;

(require 'dired)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(define-keys
 dired-mode-map
 "<DEL>" 'dired-jump
 "<mouse-2>" 'dired-find-file
 "r" 'grep-here
 "i" 'dired-find-here
 "<C-return>" 'dired-display-file)

(setq dired-auto-revert-buffer t)

(global-set-key (kbd "s-d") 'dired-jump)

(defun dired-find-here (pattern)
  "Find files in this directory using a wildcard pattern."
  (interactive
   (list
    (read-from-minibuffer
     "Find: " '("**" . 2))) )
  (find-name-dired default-directory pattern))

;;
;; completion
;;

(setq completion-styles '(initials partial-completion))

(setq completion-auto-help 'always
      completion-auto-select 'second-tab)

;; eliminates annoying delay when entering on sole completion
(setq completion-show-inline-help nil)

;;

(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
;; (global-set-key (kbd "s-w") 'bury-buffer)
(global-set-key (kbd "s-/") 'comment-dwim)

;; ergonomic copy cut paste
(global-set-key (kbd "<S-return>") 'yank)
(global-set-key (kbd "<S-backspace>") 'kill-region)
(defun maybe-copy-region ()
  (interactive)
  (if (region-active-p)
      (yank)))

(global-set-key (kbd "M-S-<down>") 'duplicate-dwim)


;;
;; mode-line
;;

(setq-default
 mode-line-format
 '((:eval (cond
           ((get-buffer-process (current-buffer))
            '(:propertize ">>>" face (:background "orange")))
           ((and (buffer-file-name) (buffer-modified-p))
            '(:propertize "+" face (:background "yellow")))))
   (:eval (or (buffer-file-name) (buffer-name)))
   ":%l:%c"))

(set-face-attribute 'mode-line nil :height 0.7)
(set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)

(set-face-attribute 'tab-line nil :height 0.8)
(set-face-attribute 'tab-bar nil :height 0.9)

;;
;;
;;

(defun insert-lambda ()
  (interactive)
  (insert "(λ ())")
  (forward-char -2))

(defun clone-sexp ()
  (interactive)
  (let* ((b (point))
         (e (save-excursion
              (forward-sexp)
              (point)))
         (s (buffer-substring-no-properties b e)))
    (insert s)
    (newline-and-indent)))

(use-package paredit
  :ensure
  :bind (:map
	 paredit-mode-map
	 ;; this one seems to be missing and results in broken syntax
	 ("C-<backspace>" . 'backward-kill-sexp)
	 ("[" . 'paredit-open-round)
	 ("]" . 'paredit-close-round)
	 ("M-c" . 'clone-sexp)
         ("C-\\" . 'insert-lambda)
	 ("{" . 'paredit-wrap-round)
	 ("}" . 'paredit-wrap-round-backwards))
  :hook emacs-lisp-mode
  :hook clojure-mode
  :hook scheme-mode)

(mapc
 (lambda (p) (put (car p) 'scheme-indent-function (cdr p)))
 '((with-mutex . 1)
   (guard . 1)
   (apply . 1)
   (match . 1)
   (match-let . 1)
   (set-fields . 1)
   (eval-when . 1)
   (with-error-to-file . 1)
   (let/ec . 1)
   (call-with-prompt . 1)
   (start-stack . 1)
   (test-assert . 1)
   (test-case . 1)
   (test-equal . 1)
   (let-assq . 2)
   (ctx . 1)
   (while . 1)
   ;; custom stuff
   (interleave . 1)
   (resolves . 1)))

;; todo binding for dragging s-exps forward and back

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

;;

(defun forward-search-region (min max)
  (interactive "r")
  (let ((str (buffer-substring-no-properties min max)))
    (unless (search-forward-regexp (regexp-quote str))
      (error "region not found"))
    (push-mark (match-beginning 0))
    (activate-mark)))

(defun backward-search-region (min max)
  (interactive "r")
  (let ((str (buffer-substring-no-properties min max)))
    (unless (save-excursion
              (goto-char min)
              (search-backward-regexp (regexp-quote str)))
      (error "region not found"))
    (goto-char (match-end 0))
    (push-mark (match-beginning 0))
    (activate-mark)))

(define-keys
 isearch-mode-map
 "s-v" 'isearch-yank-kill
 "s-r" 'isearch-repeat-backward
 "<down>" 'isearch-repeat-forward
 "<up>" 'isearch-repeat-backward)

;;

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
;; ruby
;;

(setq ruby-indent-level 4)

(defun rufo-format ()
  (interactive)
  (save-buffer)
  (let* ((file (buffer-file-name))
         (default-directory (file-name-directory file))
         (out-buffer (get-buffer-create "*rufo*")))
    (with-current-buffer out-buffer (erase-buffer))
    (let ((result
           (call-process "rufo" nil out-buffer nil "-x" file)))
      (if (equal 0 result)
          (progn
            (revert-buffer nil t t)
            (message "formatted!")
            (bury-buffer out-buffer))
        (display-buffer out-buffer)))))

;;

(load "idle-highlight.el")

;; sane scrolling
(setq mouse-wheel-scroll-amount '(2)
      mouse-wheel-progressive-speed nil)

;; (defun wip-it-good ()
;;   (interactive)
;;   (let ((dir (locate-dominating-file default-directory "test.sh")))
;;     (or dir (error "couldn't find wip script"))
;;     (message "running script")
;;     (start-process "*wip*" "*wip*" (file-name-concat dir "test.sh"))))
;; (global-set-key (kbd "<f11>") 'wip-it-good)

;;
;; mega leader map again
;;

;; (global-set-key
;;  "`"
;;  (keymap
;;   "`" 'self-insert-command
;;   "f" 'find-file
;;   "q" 'bury-buffer
;;   "k" 'kill-current-buffer
;;   "1" 'delete-other-windows
;;   "2" 'split-window-below
;;   "3" 'split-window-right
;;   ))

;;

;; make auto-revert revert faster!
(setq
 auto-revert-interval 0.5
 auto-revert--lockout-interval 0.2)

;; dirty hack to try to stop weird behaviour i'm seeing when auto-revert combined with compilation-mode
;; not sure that it does something yet...
(defun maybe-forget-errors ()
  (when (or (equal major-mode 'compilation-mode)
            (equal major-mode 'compilation-guile))
    (compilation-forget-errors)))
(add-hook 'after-revert-hook 'maybe-forget-errors)

;;

(mapc
 (lambda (s) (put s 'disabled nil))
 '(narrow-to-page
   erase-buffer
   scroll-right
   scroll-left))

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

;;

(mapc
 (lambda (s) (add-to-list 'yank-excluded-properties s))
 '(face font-lock-face))

;;

(global-set-key (kbd "<kp-add>") 'next-error)
(global-set-key (kbd "<kp-subtract>") 'previous-error)

;;

(defun call-process-buffer-replace (prog &rest args)
  "Send the entire contents of the current buffer to a command
and replace the buffer contents with the output."
  ;; FIXME doc indicates you can pass nil for START; reality indicates otherwise.
  ;; TODO only replace if return code 0; otherwise show output in error buffer
  (apply 'call-process-region (point-min) (point-max) prog t t nil args))

(defun strip-ansi-current-buffer ()
  (interactive)
  (call-process-buffer-replace "sed" "s/\x1b\[[0-9;]*m//g"))

(defun tidy-rubbish-buffer ()
  (interactive)
  (read-only-mode -1)
  (save-excursion
    (strip-ansi-current-buffer)
    (delete-trailing-whitespace)))

;;

(setq use-package-always-pin nil)

(use-package gptel)



;;

(global-set-key (kbd "<f1>") 'switch-to-buffer)
(global-set-key (kbd "<f2>") 'project-find-file)

;;

(load "../emacs/experiments/stacktrace-mode.el")
(add-to-list 'auto-mode-alist '("dump.*\\.txt" . stacktrace-mode))

;;

(setq
 split-width-threshold 1000
 split-height-threshold nil)

(blink-cursor-mode -1)

;;

(require 'yasnippet)
(require 'yasnippet-snippets)

;; TAB is already pretty overloaded, let's use backtab instead
(define-key yas-minor-mode-map (kbd "<tab>") nil t)
(define-key yas-minor-mode-map (kbd "<C-S-tab>") 'yas-expand)
(define-key yas-minor-mode-map (kbd "<backtab>") nil)
;; (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

(yas-global-mode)

;;

;; Suitable for use in display-buffer-overriding-action to inhibit buffer display.
(defun display-buffer-actually-no (&rest args) t)


(defun trigger-in-git-root ()
  "Find the Git root and tickle the trigger file.
Handy for saving everything and kicking off an external process
which can wait on the trigger file."
  (interactive)
  (save-some-buffers t)
  (let ((display-buffer-overriding-action '(display-buffer-actually-no . nil))
        (default-directory
         (locate-dominating-file default-directory ".git")))
    (shell-command "date > trigger; echo 'triggered!'")))

(global-set-key (kbd "<f11>") 'trigger-in-git-root)

;;

(setq font-lock-maximum-decoration 1)

;;

(require 'pick2)
(global-set-key (kbd "<f1>") 'pick-select-buffer)
(global-set-key (kbd "s-b") 'pick-select-buffer)
(global-set-key (kbd "<f2>") 'pick-git)
(global-set-key (kbd "s-p") 'pick-git)
(pick-define-numpad-keys)
(pick-define-function-keys)

;;

;; (defun enable-dabbrev-capf ()
;;   (add-hook 'completion-at-point-functions 'dabbrev-capf 0 t))
;; (remove-hook 'scheme-mode-hook 'enable-dabbrev-capf)
;; (remove-hook 'ruby-mode-hook 'enable-dabbrev-capf)

;;

;; (require 'corfu)

;; ;; Don't preselect anything, this means you need at least one TAB hit, which is good!
;; (setq corfu-preselect-first nil
;;       corfu-preselect 'prompt)

;; (define-keys
;;  corfu-map
;;  "<tab>" 'corfu-next
;;  "<backtab>" nil
;;  "<escape>" 'corfu-quit
;;  "<return>" nil
;;  "RET" nil
;;  ;; "M-n" 'corfu-next
;;  ;; "M-p" 'corfu-previous
;;  "M-n" nil
;;  "M-p" nil
;;  "<up>" nil
;;  "<down>" nil
;;  "C-a" nil
;;  "C-e" nil)

;; (define-key corfu-map [remap next-line] nil)
;; (define-key corfu-map [remap previous-line] nil)

;; (setq
;;  corfu-auto t
;;  corfu-auto-delay 0.2
;;  corfu-count 3
;;  corfu-bar-width 0)

;; (global-corfu-mode t)
;; (setq global-corfu-modes '((not pick-mode) t))

;; (set-face-attribute 'corfu-default nil :family "Monospace")

;; (defun buffers-in-same-mode ()
;;   (let ((mode major-mode))
;;     (seq-filter
;;      (lambda (buf) (with-current-buffer buf (eq major-mode mode)))
;;      (buffer-list))))

;; (defun just-this-buffer ()
;;   (list (current-buffer)))

;; (setq dabbrev-select-buffers-function 'buffers-in-same-mode)
;; (setq dabbrev-select-buffers-function 'just-this-buffer)

;; (defun dabbrev-capf ()
;;   "Dabbrev completion function for `completion-at-point-functions'."
;;   (or (stringp dabbrev--abbrev-char-regexp)
;;       (dabbrev--reset-global-variables))
;;   (let* ((abbrev (dabbrev--abbrev-at-point))
;;          (beg (progn (search-backward abbrev) (point)))
;;          (end (progn (search-forward abbrev) (point)))
;; 	     (ignore-case-p (dabbrev--ignore-case-p abbrev))
;; 	     (list 'uninitialized)
;;          (table
;;           (lambda (s p a)
;;             (if (eq a 'metadata)
;;                 `(metadata (cycle-sort-function . ,#'identity)
;;                            (category . dabbrev))
;;               (when (eq list 'uninitialized)
;;                 (save-excursion
;;                   ;;--------------------------------
;;                   ;; New abbreviation to expand.
;;                   ;;--------------------------------
;;                   (setq dabbrev--last-abbreviation abbrev)
;;                   ;; Find all expansion
;;                   (let ((completion-list
;;                          (dabbrev--find-all-expansions abbrev ignore-case-p))
;;                         (completion-ignore-case ignore-case-p))
;;                     (or (consp completion-list)
;;                         (user-error "No dynamic expansion for \"%s\" found%s"
;;                                     abbrev
;;                                     (if dabbrev--check-other-buffers
;;                                         "" " in this-buffer")))
;;                     (setq list
;;                           (cond
;;                            ((not (and ignore-case-p dabbrev-case-replace))
;;                             completion-list)
;;                            ((string= abbrev (upcase abbrev))
;;                             (mapcar #'upcase completion-list))
;;                            ((string= (substring abbrev 0 1)
;;                                      (upcase (substring abbrev 0 1)))
;;                             (mapcar #'capitalize completion-list))
;;                            (t
;;                             (mapcar #'downcase completion-list)))))))
;;               (complete-with-action a list s p)))))
;;     (list beg end table)))

;;

(setq revert-without-query '(".*"))

;;

(require 'bm)

(global-set-key (kbd "M-t") 'bm-toggle)
(global-set-key (kbd "M-n") 'bm-next)
(global-set-key (kbd "M-p") 'bm-previous)

(setq bm-fringe-face 'bm-fringe)
(setq bm-face 'bold)

;;

(defun yank-or-kill ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (mark) (point))
    (yank)))
(global-set-key (kbd "<mouse-3>") 'yank-or-kill)
(global-set-key (kbd "<S-mouse-3>") 'yank)

(setq mouse-yank-at-point t)

;;

(defun format-buffer ()
  (interactive)
  (cond
   ((seq-find (lambda (p) (string-prefix-p p buffer-file-name)) '("/Users/shannoncole/stile/"))
    (prettier-format))
   ((eq major-mode 'ruby-mode) (rufo-format))
   (t (error "don't know how to format"))))

(global-set-key (kbd "<f12>") 'format-buffer)

(defun prettier-format ()
  (interactive)
  (save-buffer)
  (let* ((file (buffer-file-name))
         (default-directory (or (locate-dominating-file file ".git") (error "not in git repo")))
         (out-buffer (get-buffer-create "*prettier*")))
    (with-current-buffer out-buffer (erase-buffer))
    (message "formatting with prettier...")
    (let ((result (call-process "npx" nil out-buffer nil "--prefix" "web-client" "prettier" "--write" file)))
      (if (equal 0 result)
          (progn
            (revert-buffer nil t t)
            (message "formatted!")
            (bury-buffer out-buffer))
        (display-buffer out-buffer)))))

;;

(defun copy-page ()
  (interactive)
  (save-excursion
    (mark-page)
    (kill-ring-save (point) (mark))))
(global-set-key (kbd "<f8>") 'copy-page)

;;

(setq right-margin-width 1)
(setq outline-minor-mode-use-buttons 'in-margins)

;;

(defun google (term)
  (interactive "MGoogle: ")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))

(defun context-aware-google ()
  (interactive)
  (let ((prefix
         (cond
          ((eq major-mode 'terraform-mode) "terraform")
          (else ""))))
    (google (read-from-minibuffer "Google: " prefix))))
;;

(defun close-all-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delete (current-buffer) (buffer-list))))

;;

(defun select-line-in-fringe (event)
  "Select the line where the fringe is clicked."
  (interactive "e")
  (let ((pos (posn-point (event-start event)))
        (window (posn-window (event-start event))))
    (select-window window)
    (goto-char pos)
    (push-mark (line-beginning-position) 'nomark)
    (goto-char (line-end-position))
    (activate-mark)))

(global-set-key [left-fringe mouse-1] 'select-line-in-fringe)

;;

(global-set-key (kbd "s-<mouse-1>") 'ffap-at-mouse)
(global-set-key (kbd "s-<return>") 'ffap)
