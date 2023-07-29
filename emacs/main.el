;; todo - modeline dupes filename; just show buffer name

;;
;; Handier binding
;;

;; (require 'cl)

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

(defun kill-buffer-process ()
  "Unconditionally kill any process in the current buffer."
  (interactive)
  (kill-process (get-buffer-process (current-buffer))))

;; TODO insert kill-ring into minibuffer if it starts with http?
(defun view-url (url)
  "Retrieve a URL and show it in a buffer."
  (interactive "M")
  (switch-to-buffer
   (url-retrieve-synchronously url)))

(defun close-all-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delete (current-buffer) (buffer-list))))

;;

(load "delete.el")
(text-deletion-mode 1)

(load "copy-where.el")
;; (load "grep-setup.el")
(load "idle-highlight.el")
(load "dupe-and-drag.el")
(load "notes.el")
(load "schemeing.el")
(load "search-engines.el")
(load "symbol-scan.el")
(load "time-strings.el")
(load "hopper.el")
(load "mark.el")

(load "custom-compile.el")
(load "custom-dired.el")
(load "custom-isearch.el")
(load "custom-occur.el")
(load "custom-org.el")
(load "custom-c.el")
(load "custom-org.el")
(load "custom-ruby.el")
;; (load "custom-haskell.el")
(load "custom-rust.el")
;; (load "custom-flycheck.el")
(load "custom-js.el")

(load "custom-eglot.el")
;; (load "custom-lsp.el")

;; (load "trails.el")

(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil
                    :background "#ffeeee")


;;
;; Global bindings
;;

(gsk "<kp-enter>" 'execute-extended-command)
;; (define-key minibuffer-mode-map (kbd "<kp-enter>") 'previous-line-or-history-element)

(gsk "<XF86Eject>" 'execute-extended-command)
(gsk "<S-return>" 'yank)
(gsk "<S-backspace>" 'kill-region)
(gsk "<help>" 'other-window)
(gsk "<M-f4>" 'delete-frame)
(gsk "<M-SPC>" 'cycle-spacing)
(gsk "C-x l" 'align-regexp)
(gsk "<f12>" 'save-all)
(gsk "<f5>" 'revert-buffer)

(gsk "M-z" 'zap-up-to-char)
(gsk "S-M-z" 'zap-to-char)

(defun kill-region-or-word ()
  (interactive)
  (if (region-active-p)
    (call-interactively 'kill-region)
    (call-interactively 'delete-backward-word)))

(gsk "C-w" 'kill-region-or-word)

(define-keys minibuffer-local-map
             "<escape>" 'top-level
             "<tab>" 'minibuffer-complete)

(define-keys isearch-mode-map
             "<escape>" 'isearch-exit
             "<return>" 'isearch-repeat-forward
             "<S-return>" 'isearch-repeat-backward)

(require 'rg)
(setq rg-command-line-flags '("-M" "300" "--sort" "path"))

(gsk "<f19>" 'previous-buffer)

(defun yank-or-kill ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (mark) (point))
    (yank)))
(gsk "<mouse-3>" 'yank-or-kill)
(gsk "<S-mouse-3>" 'yank)


(gsk "<C-mouse-1>" 'xref-find-definitions-at-mouse)
(gsk "C-<down-mouse-1>" nil)

(gsk "s-g" nil)


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


(gsk "<C-tab>" 'other-window)

;; todo - maybe better: compile-at-git-root
;; which also maintains a per-root buffer and selects the right one.
;; or even multi compile buffers per root; eg: (1) stack build (2) hlint.

;;
;; Buffer switching
;;

(setq ibuffer-format-save ibuffer-formats)
(setq ibuffer-formats (append ibuffer-formats '((mark " " filename-and-process))))

(defun buffer-menu-current-file ()
  "Opens the buffer menu, sorted by file, and moves point to the
current buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (let ((tabulated-list-sort-key '("File" . nil)))
      (buffer-menu))
    (goto-char (point-min))
    (while (and (not (equal (point) (point-max)))
                (not (equal buf (tabulated-list-get-id))))
      (forward-line))
    (recenter)))

(gsk "M-<f1>" 'ibuffer)

(require 'pick)
(gsk "<f1>" 'pick-select-buffer)
(pick-define-numpad-keys)
(pick-define-function-keys)

(defun pick-select-buffer-other-window ()
  (interactive)
  (let ((default-directory default-directory))
    (other-window 1)
    (pick-select-buffer nil)))

;; (gsk "<f2>" 'buffer-menu-current-file)

(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)

;;
;; Eldoc
;;

(setq
 eldoc-echo-area-use-multiline-p nil)

;;
;; Completion
;;

(setq
 completion-styles '(partial-completion flex)
 tab-always-indent 'complete)


(defun enable-dabbrev-capf () (add-to-list 'completion-at-point-functions 'cape-dabbrev))

;; (add-to-list 'completion-at-point-functions 'cape-dabbrev)


;;
;; Indenting
;;

;; TODO it looks like indent-rigidly attempts to keep the region active;
;; but it seems to fail...

(defun indent-right (beg end)
  (interactive "r")
  (let ((deactivate-mark nil))
    (indent-rigidly beg end 4)))

(defun indent-left (beg end)
  (interactive "r")
  (let ((deactivate-mark nil))
    (indent-rigidly beg end -4)))

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
 fill-column 80
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %f:%l:%c"))

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

(when (boundp 'terminal-prog)
  (defun term-here ()
    (interactive)
    (start-process "term" nil terminal-prog)))


;;
;; Horizontal scrolling
;;

;; (defun small-scroll-right () (interactive) (scroll-right 5 nil))
;; (defun small-scroll-left () (interactive) (scroll-left 5 nil))
;; (gsk "<C-prior>" 'small-scroll-right)
;; (gsk "<C-next>" 'small-scroll-left)

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
                    str nil nil nil nil))
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
;; Browsing back & forth in directory order.
;;

;; TODO this should probably use dired so the order of files matches
(defun find-next-file (&optional offset)
  "Find a file in order relative to the current
file based on OFFSET."
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

(gsk "C-x <down>" 'find-next-file)
(gsk "C-x <up>" 'find-prev-file)

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

;; TODO a simple CSV mode
(defun align-buffer-commas ()
  (interactive "")
  (align-regexp (point-min) (point-max)
                "\\( *\\)," 1 1 t))

;;

;; (defun select-whole-line ()
;;   (interactive)
;;   (deactivate-mark)
;;   (beginning-of-line)
;;   (activate-mark)
;;   (end-of-line))

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
  (shell-command-on-region beg end "jq" nil t))

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



;;

;; (require 'swiper)

;; (defun swiper-selection ()
;;   (interactive)
;;   (if (region-active-p)
;;       (let ((str (regexp-quote (buffer-substring-no-properties (point) (mark)))))
;;         (deactivate-mark)
;;         (swiper str))
;;     (swiper)))

;; (gsk "C-'" 'swiper-selection)
;; (gsk "<kp-5>" 'swiper-selection)

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

;; (gsk "<kp-decimal>" 'highlight-symbol-at-point)

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
(gsk "<S-C-tab>" 'yas-expand)

;;

(defun tab-bar-name-first-window ()
  (truncate-string-to-width
   (buffer-name (window-buffer (frame-first-window)))
   tab-bar-tab-name-truncated-max nil nil "…"))

(setq
 tab-bar-close-button-show nil
 ;; tab-bar-tab-name-function 'tab-bar-name-first-window
 tab-bar-tab-name-function 'tab-bar-tab-name-current
 tab-bar-tab-name-truncated-max 20
 tab-bar-show 1)


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
 nil
 )

(defun dedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) t)
  (message "this window is now dedicated"))

(defun undedicate-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (message "this window is no longer dedicated"))

;;

(gsk "C-<kp-add>" 'flymake-goto-next-error)
(gsk "C-<kp-subtract>" 'flymake-goto-prev-error)

(gsk "C-M-n" 'flymake-goto-next-error)
(gsk "C-M-p" 'flymake-goto-prev-error)

;;

(setq auto-save-visited-interval 1)
(auto-save-visited-mode -1)

;;

(defun mark-sexp-forward ()
  (interactive)
  (unless (region-active-p)
    (set-mark (point)))
  (forward-sexp))

;; (gsk "<tab>" 'mark-sexp-forward)

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
  "!" 'sh-region)

(selected-global-mode)

;;
;; Mega leader map
;;

(gsk "C-<escape>" 'dired-jump)

(gsk
 "<escape>"
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
  "<escape>" 'dired-jump
  "`" 'buffer-menu-current-file
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
              "c" 'flymake-mode)
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

(rg-define-search rg-dired :dir current)
(rg-define-search rg-project-all :dir project :files "*")
;; (rg-define-search rg-all :files "*")

(defun rg-region-immediate ()
  (interactive)
  (if (region-active-p)
    (let ((q (buffer-substring-no-properties (point) (mark))))
      (rg-project q "*"))
    (call-interactively 'rg-project-all)))

(gsk "s-F" 'rg-region-immediate)

(define-keys
 dired-mode-map
 "<left>" 'dired-jump
 "<right>" 'dired-find-file
 "r" 'rg-dired)

;;

(setq completion-styles '(partial-completion flex))

;; todo when emacs 29, can use a built-in
(require 'cape)
(defun enable-dabbref-capf ()
  (add-to-list 'completion-at-point-functions 'cape-dabbrev))

(add-hook 'c-mode-hook 'enable-dabbref-capf)
(add-hook 'c-mode-hook 'corfu-mode)

;;

(require 'corfu)

(define-keys corfu-map
  "<tab>" 'corfu-complete
  "<escape>" 'corfu-quit
  "<return>" nil
  "RET" nil
  "M-n" 'corfu-next
  "M-p" 'corfu-previous
  "<up>" nil
  "<down>" nil)
(define-key corfu-map [remap next-line] nil)
(define-key corfu-map [remap previous-line] nil)

(setq
 tab-always-indent 'complete
 corfu-auto t
 corfu-auto-delay 0.1
 corfu-count 5)

(add-hook 'emacs-lisp-mode-hook 'corfu-mode)
(add-hook 'terraform-mode-hook 'corfu-mode)
(add-hook 'text-mode-hook 'corfu-mode)
(add-hook 'markdown-mode-hook 'corfu-mode)
(add-hook 'js-mode-hook 'corfu-mode)

(add-hook 'ruby-mode-hook 'corfu-mode)
(add-hook 'ruby-mode-hook 'enable-dabbrev-capf)


(set-face-attribute 'corfu-default nil :family "Monospace" :height 0.95)

;;

(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)

;;

;; (gsk "M-<right>" 'forward-sexp)
;; (gsk "M-<left>" 'backward-sexp)
;; (gsk "M-<up>" 'backward-up-list)
;; (gsk "M-<down>" 'down-list)


(set-face-attribute
 'mode-line nil
 :inherit 'variable-pitch)

(set-face-attribute
 'mode-line-inactive nil
 :weight 'unspecified)

;;

;; todo use overlays to make it visible
;; todo use a global hash so you can rever the buffer and not lose marks

(defvar-local manual-marks '())
(defun manual-mark-toggle ()
  (interactive)
  (let ((p (point-marker)))
    (if (seq-contains-p manual-marks p (lambda (a b) (eq (marker-position a) (marker-position b))))
        (progn
          (setq manual-marks (delete p manual-marks))
          (message "marker removed"))
      (progn
        (add-to-list 'manual-marks p)
        (sort manual-marks '<)
        (message "marker added")))))
(defun manual-mark-next ()
  (interactive)
  (let* ((p (point))
         (target (car (seq-filter (lambda (x) (> (marker-position x) p)) manual-marks))))
    (goto-char (marker-position target))))
(defun manual-mark-prev ()
  (interactive)
  (let* ((p (point))
         (target (car (seq-filter (lambda (x) (< (marker-position x) p)) (reverse manual-marks)))))
    (goto-char (marker-position target))))

(gsk "C-/" 'manual-mark-toggle)
(gsk "C-." 'manual-mark-next)
(gsk "C-," 'manual-mark-prev)

;; (gsk "<kp-enter>" 'manual-mark-toggle)
;; (gsk "<kp-add>" 'manual-mark-next)
;; (gsk "<kp-subtract>" 'manual-mark-prev)

;;
;;
;;

;; todo turns out this is way too intrustive, i always accidentally type S-space

;; (defun activate-mark-select-forward-word ()
;;   (interactive)
;;   (unless mark-active (set-mark (point)) (activate-mark))
;;   (forward-word))

;; (define-key dired-mode-map (kbd "<S-SPC>") nil)
;; (gsk "<S-SPC>" 'activate-mark-select-forward-word)

(defun select-this-line ()
  (interactive)
  (beginning-of-line)
  (set-mark (pos-bol))
  (forward-line 1)
  (activate-mark))
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
