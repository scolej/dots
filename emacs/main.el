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

(defun gsk (k f)
  (global-set-key (kbd k) f))

;;
;; Wrappers, shortcuts, utilities
;;

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(defun really-kill-buffer ()
  "Unconditionally kill the current buffer."
  (interactive) (kill-buffer nil))

(global-set-key (kbd "C-x k") 'really-kill-buffer)

(defun kill-buffer-process ()
  "Unconditionally kill any process in the current buffer."
  (interactive)
  (kill-process (get-buffer-process (current-buffer))))

(defun close-all-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delete (current-buffer) (buffer-list))))

;;

(load "delete.el")
(text-deletion-mode 1)

(load "copy-where.el")
(load "idle-highlight.el")
(load "dupe-and-drag.el")
(load "notes.el")
(load "schemeing.el")
(load "search-engines.el")
(load "symbol-scan.el")
(load "time-strings.el")
(load "hopper.el")
(load "mark.el")
(load "grep2.el")

(load "custom-compile.el")
(load "custom-dired.el")
(load "custom-isearch.el")
(load "custom-occur.el")
(load "custom-org.el")
(load "custom-c.el")
(load "custom-org.el")
(load "custom-ruby.el")
(load "custom-rust.el")
(load "custom-js.el")
(load "custom-haskell.el")

(load "custom-eglot.el")

(setq-default show-trailing-whitespace nil)
(set-face-attribute 'trailing-whitespace nil
                    :background "#fff6f6")

;;
;; Global bindings
;;

(gsk "<kp-enter>" 'execute-extended-command)

(gsk "<XF86Eject>" 'swiper)
(gsk "<help>" 'swiper)
(gsk "s-f" 'swiper)

(gsk "<S-return>" 'yank)
(gsk "<S-backspace>" 'kill-region)

;; (gsk "<help>" 'other-window)
(gsk "<M-f4>" 'delete-frame)

(gsk "<f5>" 'revert-buffer)
(gsk "C-;" 'comment-line)

(gsk "M-z" 'zap-up-to-char)
(gsk "S-M-z" 'zap-to-char)

(gsk "M-1" 'delete-other-windows)
(gsk "M-2" 'split-window-below)
(gsk "M-3" 'split-window-right)
(gsk "M-0" 'delete-window)

(gsk "<C-M-up>" 'backward-up-list)
(gsk "<C-M-down>" 'smie-down-list)

(gsk "<M-left>" 'backward-word)
(gsk "<M-right>" 'forward-word)
(gsk "<M-delete>" 'delete-forward-word)

;;

(defun kill-region-or-word ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'delete-backward-word)))

(gsk "C-w" 'kill-region-or-word)

;;

(define-keys minibuffer-local-map
             "<escape>" 'top-level
             "<tab>" 'minibuffer-complete)

(require 'isearch)
(define-keys isearch-mode-map
             "<escape>" 'isearch-exit
             "<down>" 'isearch-repeat-forward
             "<up>" 'isearch-repeat-backward)

;;

(defun yank-or-kill ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (mark) (point))
    (yank)))
(gsk "<mouse-3>" 'yank-or-kill)
(gsk "<S-mouse-3>" 'yank)

;;

(gsk "<C-mouse-1>" 'xref-find-definitions-at-mouse)
(gsk "C-<down-mouse-1>" nil)

;;

(gsk "C-x C-t" 'tab-new)
(gsk "s-t" 'tab-new)
(gsk "s-w" 'tab-close)
(gsk "C-<next>" 'tab-bar-switch-to-next-tab)
(gsk "C-<prior>" 'tab-bar-switch-to-prev-tab)
(gsk "C-S-<next>" 'tab-bar-move-tab)
(gsk "C-S-<prior>" 'tab-bar-move-tab-backward)

(gsk "C-<wheel-down>" 'tab-bar-switch-to-next-tab)
(gsk "C-<wheel-up>" 'tab-bar-switch-to-prev-tab)
(gsk "C-S-<wheel-right>" 'tab-bar-move-tab)
(gsk "C-S-<wheel-left>" 'tab-bar-move-tab-backward)

(gsk "<f15>" 'tab-bar-switch-to-next-tab)
(gsk "<f14>" 'tab-bar-switch-to-prev-tab)


;;

(gsk "<C-tab>" 'other-window)
(gsk "<C-S-tab>" nil)
(gsk "<f13>" 'other-window)

;; todo - maybe better: compile-at-git-root
;; which also maintains a per-root buffer and selects the right one.
;; or even multi compile buffers per root; eg: (1) stack build (2) hlint.

;;
;; Buffer switching
;;

(require 'pick2)
(gsk "<f1>" 'pick-select-buffer)
(gsk "<f2>" 'pick-git)
(pick-define-numpad-keys)
(pick-define-function-keys)

;;

(setq eldoc-echo-area-use-multiline-p t)

;;
;; Completion
;;

(setq
 completion-styles '(partial-completion flex)
 tab-always-indent t)

;; todo when emacs 29, can use a built-in
(require 'cape)

(defun enable-dabbrev-capf ()
  (add-hook 'completion-at-point-functions 'cape-dabbrev 0 t))

(add-hook 'c-mode-hook 'enable-dabbrev-capf)
(add-hook 'ruby-mode-hook 'enable-dabbrev-capf)
(add-hook 'haskell-mode-hook 'enable-dabbrev-capf)
(add-hook 'scheme-mode-hook 'enable-dabbrev-capf)
;; (remove-hook 'emacs-lisp-mode-hook 'enable-dabbrev-capf)

;;

(setq c-basic-offset 4)

;;

(defun clone-region (beg end)
  (interactive "r")
  (let ((deactivate-mark nil)
        (str (buffer-substring-no-properties beg end)))
    (save-excursion
      (goto-char end)
      (insert str))))

;;

(defun surround-region (before &optional after)
  (interactive "M")
  (save-excursion
    (goto-char (min (point) (mark)))
    (insert before))
  (save-excursion
    (goto-char (max (point) (mark)))
    (insert (or after before))))

;;

(setq-default
 fill-column 90
 mode-line-format
 '((:eval (cond
           ((get-buffer-process (current-buffer))
            '(:propertize ">>>" face (:background "orange")))
           ((and (buffer-file-name) (buffer-modified-p))
            '(:propertize "+" face (:background "yellow")))))
   (:eval (or (buffer-file-name) (buffer-name)))
   ":%l:%c"))

(setq
 next-screen-context-lines 2
 hi-lock-auto-select-face t
 Info-isearch-search t
 Info-use-header-line nil)

(mapc
 (lambda (s) (add-to-list 'yank-excluded-properties s))
 '(face font-lock-face))

;; Enable useful disabled things.
(mapc
 (lambda (s) (put s 'disabled nil))
 '(narrow-to-page
   erase-buffer
   scroll-right
   scroll-left))

;;
;; Horizontal scrolling
;;

(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

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
;; Opening lines
;;

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

(defun open-line-below-and-indent ()
  (interactive)
  (newline 2)
  (forward-line -1)
  (funcall indent-line-function))

(gsk "C-S-o" 'new-line-above)
(gsk "C-o" 'new-line-below)

;;

(defvar eol-bol-rep-last-dir nil
  "Last direction moved by bol-and-advance or eol-and-advance. 1
  indicates forwards, -1 indicates backwards.")

(defun bol-and-advance (arg)
  "Move to the beginning of line. If we're already there, move
either forwards or backwards by one line, depending on the prefix
arg and the last action. Use a prefix arg to move forward. The
motion may be repeated without repeating the prefix arg."
  (interactive "P")
  (let* ((reverse (or arg
                      (and
                       (equal last-command 'bol-and-advance)
                       (equal eol-bol-rep-last-dir 1))))
         (dir (if reverse 1 -1)))
    (setq eol-bol-rep-last-dir dir)
    (if (= (point-at-bol) (point))
        (forward-line dir)))
  (beginning-of-line))

(defun eol-and-advance (arg)
  "Move to the end of line. If we're already there, move either
forwards or backwards by one line, depending on the prefix arg
and the last action. Use a prefix arg to move forward. The motion
may be repeated without repeating the prefix arg."
  (interactive "P")
  (let* ((reverse (or arg
                      (and
                       (equal last-command 'eol-and-advance)
                       (equal eol-bol-rep-last-dir -1))))
         (dir (if reverse -1 1)))
    (setq eol-bol-rep-last-dir dir)
    (if (= (point-at-eol) (point))
        (forward-line dir)))
  (end-of-line))

(gsk "C-e" 'eol-and-advance)
(gsk "C-a" 'bol-and-advance)

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

(setq scroll-preserve-screen-position nil)

;;

(defun pp-json (beg end)
  (interactive "r")
  (unless (region-active-p) (mark-sexp))
  (shell-command-on-region beg end "jq" nil t))

(defun pp-json-other-window ()
  (interactive)
  (let ((p1 (point))
        (p2 (save-excursion (forward-sexp) (point)))
        (buf (get-buffer-create "*yq*"))
        (inhibit-message t)) ;; don't show the output in the echo area, regardless of how small it is
    (shell-command-on-region p1 p2 "yq -P -p json" buf)
    (pop-to-buffer buf)))

(defun pp-json-eol-other-window ()
  (interactive)
  (let ((p1 (save-excursion (beginning-of-line) (re-search-forward "{") (1- (point))))
        (p2 (pos-eol))
        (buf (get-buffer-create "*yq*"))
        (inhibit-message t)) ;; don't show the output in the echo area, regardless of how small it is
    (shell-command-on-region p1 p2 "yq -P -p json" buf)
    (pop-to-buffer buf)))

(defun pp-escaped-json (beg end)
  (interactive "r")
  (let* ((orig (buffer-substring-no-properties beg end))
         (unescaped (replace-regexp-in-string
                     (regexp-quote "\\\\") "\\"
                     (replace-regexp-in-string
                      (regexp-quote "\\\"") "\""
                      orig))))
    (delete-region beg end)
    (insert unescaped)
    (pp-json beg (point))))

;; todo dupe-to-other-window

;;

(require 'help-mode)

(defun revert-help-no-confirm ()
  (interactive)
  (help-mode-revert-buffer nil t))

(define-key help-mode-map (kbd "g") 'revert-help-no-confirm)

;;

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(define-derived-mode colour-log-view-mode fundamental-mode "colour log view mode"
  (display-ansi-colors)
  (view-mode)
  ;; dissociate from file; we don't want to write any changes.
  (setq buffer-file-name nil))

;; (add-to-list 'auto-mode-alist '("\\.log\\'" . colour-log-view-mode))
;; (setq auto-mode-alist (remove '("\\.log\\'" . colour-log-view-mode) auto-mode-alist))

;;

(setq hi-lock-face-defaults
      '("hi-yellow" "hi-pink" "hi-green"
        "hi-blue" "hi-salmon" "hi-aquamarine"
        "hi-black-b" "hi-blue-b" "hi-red-b"
        "hi-green-b"))

;;
;; Handy word motion complements.
;;

(defun backward-end-of-word ()
  (interactive)
  (cl-flet ((go (lambda () (re-search-backward "\\>" nil t))))
    (when (= (point) (go))
      (left-char)
      (go))))

(defun forward-start-of-word ()
  (interactive)
  (cl-flet ((go (lambda () (re-search-forward "\\<" nil t))))
    (when (= (point) (go))
      (right-char)
      (go))))

(gsk "M-B" 'backward-end-of-word)
(gsk "M-F" 'forward-start-of-word)

;;

(defun find-parent-readme ()
  (interactive)
  (find-file
   (concat
    (locate-dominating-file default-directory "README.md")
    "README.md")))

;;

(require 'yasnippet)
(require 'yasnippet-snippets)

;; TAB is already pretty overloaded, let's use backtab instead
(define-key yas-minor-mode-map (kbd "<tab>") nil t)
(define-key yas-minor-mode-map (kbd "<C-S-tab>") 'yas-expand)

(yas-global-mode)

;;

(defun tab-bar-name-first-window ()
  (truncate-string-to-width
   (buffer-name (window-buffer (frame-first-window)))
   tab-bar-tab-name-truncated-max nil nil "…"))

(setq
 tab-bar-close-button-show nil
 tab-bar-tab-name-function 'tab-bar-name-first-window
 ;; tab-bar-tab-name-function 'tab-bar-tab-name-current
 ;; tab-bar-tab-name-truncated-max 15
 tab-bar-show 1
 tab-bar-auto-width nil
 tab-bar-auto-width-min 10
 tab-bar-auto-width-max 20)

;;

;; todo this should be a list so we can find the first valid one, that way
;; you can set one per tab?
(defvar working-win nil)

(defun mark-this-as-working-win ()
  (interactive)
  (setq working-win (selected-window)))

;; todo - still some issues with `next-error` which seems to make TWO calls to display-buffer
;; todo - also what about find-file - it gets derailed by this
(defun display-buffer-in-working-win (buffer alist)
  (let ((already (get-buffer-window buffer t)))
    (cond
     (already (select-window already))
     ((and working-win (window-valid-p working-win))
      (set-window-buffer working-win buffer)))))

(setq
 display-buffer-alist
 ;; '((".*" . (display-buffer-in-working-win . ())))
 nil)

(defun dedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) t)
  (message "this window is now dedicated"))

(defun undedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (message "this window is no longer dedicated"))

;;
;; Selected
;;

(require 'selected)

(define-keys selected-keymap
  "<return>" 'kill-ring-save
  "r" 'query-replace-maybe-region
  "k" 'idle-highlight-keep
  "i" 'indent-rigidly
  ";" 'comment-dwim
  "'" 'swiper-selection
  "S" 'sort-lines
  "s" 'surround-region
  "o" 'occur-selection
  ;; "<tab>" 'indent-right
  ;; "<backtab>" 'indent-left
  ;; "<tab>" nil
  ;; "<backtab>" nil
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
  "e" 'eval-region
  "<escape>" 'keyboard-quit
  "!" 'sh-region
  "m" 'copy-crumb
  "<C-down>" 'forward-search-region
  "<C-up>" 'backward-search-region
  )

(selected-global-mode)

;;
;; Mega leader map
;;

;; todo - when i'm reverse-searching in the minibuffer i want to see the full search string that i've typed!

(gsk "C-<escape>" 'dired-jump)
;; (gsk "<escape>" 'top-level)

(gsk
 "<f9>"
 (keymap
  "DEL" 'dired-jump
  "k" 'really-kill-buffer
  "e" 'eval-buffer
  "c" 'make-frame
  "q" 'quit-window
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right
  "=" 'balance-windows
  "f" 'find-file
  "F" 'file-hopper
  "g" 'rg
  "G" 'rg-project-all
  "b" 'switch-to-buffer
  "s" (keymap "g" 'google
              "s" 'stackoverflow
              "t" 'teclis
              "r" 'rust-core)
  "h" (keymap "f" 'describe-function
              "v" 'describe-variable
              "k" 'describe-key
              "m" 'describe-mode
              "i" 'info
              "a" 'apropos)
  "<left>" 'previous-buffer
  "<right>" 'next-buffer
  "<escape>" 'keyboard-quit
  "`" (lambda () (interactive) (insert "`"))
  "n" 'next-error
  "p" 'previous-error
  "o" 'occur
  "y" (keymap "c" 'copy-crumb
              "g" 'copy-git-buffer-path
              "b" 'copy-buffer-path
              "n" 'copy-buffer-path-and-line)
  "t" (keymap "l" 'visual-line-mode
              "n" 'linum-mode
              "f" 'auto-fill-mode
              "r" 'refill-mode
              "c" 'flycheck-mode)
  "v" 'view-mode
  "j" (keymap "b" 'bk-bfp-branch
              "n" 'take-notes
              "N" 'jump-to-notes-dir
              "g" 'github-current-branch
              "j" 'browse-url-at-point
              "D" (lambda () (interactive) (find-file "~/Downloads"))
              "d" 'jump-to-dev-env
              ;; "a" aws-jumper-keymap
              )
  "C" 'compile-in-dir
  "w" (keymap "d" 'dedicate-window
              "w" 'mark-this-as-working-win)
  "r" 'query-replace-resume
  "d" (keymap "p" 'profiler-start)
  "x" 'delete-trailing-whitespace
  "<f1>" 'pick-select-buffer-other-window
  "!" 'sh-region
  ))

;;

;; (require 'rg)

;; (setq rg-command-line-flags '("-M" "300" "--sort" "path"))

;; (rg-define-search rg-dired :dir current)
;; (rg-define-search rg-project-all :dir project :files "*")
;; ;; (rg-define-search rg-all :files "*")

;; ;; (defun rg-name-function ()
;; ;;   ;; (format "*rg: %s*" (gethash :pattern rg-cur-search))
;; ;;   (format "*rg: %s*" pattern))
;; ;; (setq rg-buffer-name nil)

;; (defun rg-dwim (prefix)
;;   (interactive "P")
;;   (if prefix (call-interactively 'rg)
;;     (if (region-active-p)
;;         (let ((q (buffer-substring-no-properties (point) (mark))))
;;           (rg-project q "*"))
;;       (call-interactively 'rg-project-all))))

;; (gsk "s-g" 'rg-dwim)
;; (gsk "s-G" 'rg)

(define-keys
 dired-mode-map
 "r" 'grep-here)

;;

(require 'corfu)

;; Don't preselect anything, this means you need at least one TAB hit, which is good!
(setq corfu-preselect-first nil
      corfu-preselect 'prompt)

(define-keys
 corfu-map
 "<tab>" 'corfu-next
 "<backtab>" nil
 "<escape>" 'corfu-quit
 "<return>" nil
 "RET" nil
 ;; "M-n" 'corfu-next
 ;; "M-p" 'corfu-previous
 "M-n" nil
 "M-p" nil
 "<up>" nil
 "<down>" nil
 "C-a" nil
 "C-e" nil)

(define-key corfu-map [remap next-line] nil)
(define-key corfu-map [remap previous-line] nil)

(setq
 corfu-auto t
 corfu-auto-delay 0.2
 corfu-count 3
 corfu-bar-width 0
 corfu-auto-delay 0.15
 corfu-count 3)

(global-corfu-mode t)
(setq global-corfu-modes '((not pick-mode) t))

(set-face-attribute 'corfu-default nil :family "Monospace")

;;

(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)

;;

(defun server-edit-save-first () (interactive) (save-buffer) (call-interactively 'server-edit))
(gsk "C-x #" 'server-edit-save-first)

;;

(set-face-attribute
 'mode-line nil
 :inherit 'variable-pitch)

(set-face-attribute
 'mode-line-inactive nil
 :weight 'unspecified)

;;

(defun select-this-line ()
  (interactive)
  (if (region-active-p) (forward-line 1)
    (beginning-of-line)
    (set-mark (pos-bol))
    (forward-line 1)
    (activate-mark)))

(gsk "<M-SPC>" 'select-this-line)

;;

(defun sh-region ()
  (interactive)
  (let ((beg) (end))
    (if (region-active-p)
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (progn
          (setq beg (pos-bol))
          (setq end (pos-eol))))
    (let ((txt (buffer-substring-no-properties beg end)))
      (deactivate-mark)
      (goto-char end)
      (end-of-line)
      (newline)
      (shell-command txt (current-buffer)))))

;; TODO
;;
;; buffer rings. a file with lists of filepaths; each paragraph is a group that you can easily loop through.
;;
;; could paste in a list of git conflicts and then cycle through all the files.


;; TODO
;;
;;  define a new ibuffer col that's either the full filename or buffer name
;;

(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;;

(defun bring-and-insert-image (orig new)
  (interactive "fImage file:\nMNew name:")
  (copy-file orig (file-name-concat default-directory new) 1)
  (insert new))

;;

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; todo would love it if this became disabled whenever i manualyl resized
;; and then i could turn it back on with esc =

(let ((colour "#f200ff"))
  ;; todo why do i need both?
  ;; this still is not reliable!!! wtf?
  (set-cursor-color colour)
  (set-face-attribute
   'cursor nil
   :background colour))

;;

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

(defun ormolu-format ()
  (interactive)
  (save-buffer)
  (let* ((file (buffer-file-name))
         (default-directory (file-name-directory file))
         (out-buffer (get-buffer-create "*ormolu*")))
    (with-current-buffer out-buffer (erase-buffer))
    (let ((result
           (call-process "ormolu" nil out-buffer nil "-i" file)))
      (if (equal 0 result)
          (progn
            (revert-buffer nil t t)
            (message "formatted!")
            (bury-buffer out-buffer))
        (display-buffer out-buffer)))))

;; (defun rubocop-format ()
;;   (save-buffer)
;;   (let* ((file (buffer-file-name))
;;          (default-directory (file-name-directory file))
;;          (out-buffer (get-buffer-create "*rubocop*")))
;;     (with-current-buffer out-buffer (erase-buffer))
;;     (let ((result
;;            (call-process "rubocop" nil out-buffer nil "-d" "-x" file)
;;                                         ;(call-process "pwd" nil out-buffer nil)
;;            ))
;;       (if (equal 0 result)
;;           (progn
;;             (revert-buffer nil t t)
;;             (message "formatted!")
;;             (bury-buffer out-buffer))
;;         (display-buffer out-buffer)))))

(defun format-buffer ()
  (interactive)
  (cond
   ((seq-find (lambda (p) (string-prefix-p p buffer-file-name))
              '("/Users/shannoncole/stile/dev-environment"
                "/Users/shannoncole/stile/dev-ref"
                "/Users/shannoncole/stile/master"))
    (prettier-format))
   ((eq major-mode 'ruby-mode) (rufo-format))
   ((eq major-mode 'haskell-mode) (ormolu-format))
   (t (error "don't know how to format"))))

(gsk "<f12>" 'format-buffer)

;;

(gsk "M-u" 'er/expand-region)

;;

(require 'swiper)
(gsk "M-o" 'swiper)
;; (gsk "`" 'swiper)
(define-key swiper-map (kbd "<escape>") 'top-level)

;;

(gsk "C-x +" 'global-text-scale-adjust)
(gsk "C-x -" 'global-text-scale-adjust)

;;

;; I'll often copy text by whole lines, which means the first line has a chunk
;; of indentation at the start. When pasting that text, if point is already
;; indented, it's extremely unlikely that we want to include the copied
;; whitespace when pasting, so just strip it.
;;
;; TODO: also strip the common prefix from multi lines
(defun leading-whitespace-stripper (text)
  (if (eq (point) (pos-bol)) text
    (string-trim-left text)))

(add-to-list 'yank-transform-functions 'leading-whitespace-stripper)

;;

;; (gsk "M-n" 'next-error)
;; (gsk "M-p" 'previous-error)

;;

(defun copy-whole-buffer ()
  (interactive)
  (copy-region-as-kill (point-min) (point-max))
  (message "Copied the whole visible buffer!"))
(gsk "<f19>" 'copy-whole-buffer)
(gsk "<f8>" 'copy-whole-buffer)

;;

(setq eldoc-documentation-function 'eldoc-documentation-compose)
(global-eldoc-mode 1)
(setq max-mini-window-height 0.5)

;;

(load "experiments/stacktrace-mode.el")

;;

(defun beginning-of-line-toggle ()
  (interactive)
  (if (eq (- (point) (pos-bol)) (current-indentation))
      (beginning-of-line)
    (back-to-indentation)))

(gsk "<home>" 'beginning-of-line-toggle)

;;

;; (defun at-word-boundary ()
;;   (or (and (= (char-syntax (char-after)) ?w)
;;            (not (= (char-syntax (char-before)) ?w)))
;;       (and (= (char-syntax (char-before)) ?w)
;;            (not (= (char-syntax (char-after)) ?w)))))

;; (defun forward-word-boundary ()
;;   (interactive)
;;   (while (progn (forward-char) (not (at-word-boundary)))))

;; (defun backward-word-boundary ()
;;   (interactive)
;;   (while (progn (backward-char) (not (at-word-boundary)))))

;; (global-set-key [C-right] 'forward-word-boundary)
;; (global-set-key [C-left]  'backward-word-boundary)

(global-set-key [C-right] 'forward-word)
(global-set-key [C-left]  'backward-word)

;;

(require 'markdown-mode)
(set-face-attribute
 'markdown-header-face-1 nil
 :height 1.5)
(set-face-attribute
 'markdown-header-face-2 nil
 :height 1.3)
(set-face-attribute
 'markdown-header-face-3 nil
 :height 1.2)

;;
;;
;;

(defun recompile-dwim ()
  (interactive)
  (let* ((window-filter
          ;; Filter to find the first window which is derived from compilation-mode
          (lambda (win)
             (and (window-live-p win)
                  (with-current-buffer (window-buffer win)
                    (and (derived-mode-p 'compilation-mode)
                         ;; sometimes we have a compilation-mode buffer that wasn't
                         ;; actually the result of running a compilation (eg: I just
                         ;; enabled it to get file link buttons). We don't want to find
                         ;; this sort of buffer because it will trigger a new
                         ;; compilation.
                         compilation-arguments)))))
         (win (get-window-with-predicate window-filter)))
    (unless win (error "no compilation here"))
    (with-selected-window win (recompile))))

(gsk "<f11>" 'recompile-dwim)

;;

(defun copy-ampy-block ()
  (interactive)
  (save-excursion
    (let ((start) (end))
      (re-search-backward "&&&")
      (forward-line)
      (setf start (point))
      (re-search-forward "&&&")
      (beginning-of-line)
      (setf end (point))
      (copy-region-as-kill start end))))

(gsk "<f8>" 'copy-ampy-block)

;;

(window-divider-mode 1)
(setq window-divider-default-right-width 4)
