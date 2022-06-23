;; todo - modeline dupes filename; just show buffer name

;; todo
;;
;; - pressing M-j should preserve space after comment
;;
;; - M-j should work in haskell-mode

;;
;; Handier binding
;;

(require 'cl)

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
(defun copy-crumb ()
  "Copy file path, line number, and trimmed line."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (error "not in a git repo")))
         (abs (or (buffer-file-name)
                  (default-directory)))
         (line (s-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
         (linum (number-to-string (line-number-at-pos (point))))
         (str1 (concat (file-relative-name abs root) ":" linum " " line))
         (str2 (concat str1 "\n")))
    (kill-new str2)
    (let ((x-select-enable-primary t))
      (x-select-text str2))
    ;; todo crumbs with %s in them !?
    (message (format "Copied crumb: %s" str1))))

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

;;

(load "grep-setup.el")
(load "idle-highlight.el")
(load "trails.el")
;; (load "delete.el") todo this fights the paredit binds :(
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
(load "custom-haskell.el")
(load "custom-flycheck.el")

;;
;; Global bindings
;;

(gsk "<kp-enter>" 'execute-extended-command)
(gsk "<XF86Eject>" 'execute-extended-command)
(gsk "<S-return>" 'yank)
(gsk "<C-tab>" 'other-window)
(gsk "<M-f4>" 'delete-frame)
(gsk "<M-SPC>" 'cycle-spacing)
(gsk "C-x l" 'align-regexp)
(gsk "<f12>" 'save-all)
(gsk "<f5>" 'revert-buffer)

(define-keys minibuffer-local-map
  "<escape>" 'top-level
  "<tab>" 'minibuffer-complete)

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
  "g" 'git-grep-symbol-at-point
  "G" 'git-grep-root-symbol-at-point
  "b" 'switch-to-buffer
  "s" (keymap "l" 'highlight-lines-matching-regexp
              "r" 'highlight-regexp
              "u" 'unhighlight-regexp)
  "h" (keymap "f" 'describe-function
              "v" 'describe-variable
              "k" 'describe-key
              "m" 'describe-mode
              "i" 'info
              "a" 'apropos)
  "<left>" 'previous-buffer
  "<right>" 'next-buffer
  "<escape>" 'explore-mode
  "`" 'buffer-menu-current-file
  "n" 'next-error
  "p" 'previous-error
  "o" 'occur
  "y" (keymap "c" 'copy-crumb)
  "t" (keymap "l" 'visual-line-mode
              "n" 'linum-mode
              "f" 'auto-fill-mode
              "c" 'flycheck-mode)
  "v" 'view-mode
  "j" (keymap "b" 'bk-bfp-branch
              "n" 'take-notes
              "N" 'jump-to-notes-dir
              "g" 'github-current-branch)
  "C" 'compile))

;; todo - maybe better: compile-at-git-root
;; which also maintains a per-root buffer and selects the right one.
;; or even multi compile buffers per root; eg: (1) stack build (2) hlint.

;;
;; Buffer switching
;;

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

;; doesn't work :(
;; (defun buffer-menu-toggle-sort ()
;;   (interactive)
;;   (setq-local
;;    tabulated-list-sort-key
;;    (if (and (listp tabulated-list-sort-key)
;;             (equal "File" (car tabulated-list-sort-key)))
;;        '("C" . nil)
;;      '("File" . nil)))
;;   (tabulated-list-init-header)
;;   (tabulated-list-print t))
;; (define-key Buffer-menu-mode-map "<f1>" 'buffer-menu-toggle-sort)

(require 'pick)
(gsk "<f1>" 'pick-select-buffer)
(gsk "<kp-1>" 'pick-select-buffer)
(pick-define-numpad-keys)
(pick-define-function-keys)

(gsk "<f2>" 'buffer-menu-current-file)

(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)

;;
;; Completion
;;

(setq completion-styles '(partial-completion))

(require 'company)
(global-company-mode 1)
(setq company-idle-delay nil)

(define-keys company-mode-map
  "<M-tab>" 'company-complete)

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

;;

(defun clone-region (beg end)
  (interactive "r")
  (let ((deactivate-mark nil)
        (str (buffer-substring-no-properties beg end)))
    (save-excursion
      (goto-char end)
      (insert str))))

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
  "o" 'occur-selection
  "<tab>" 'indent-right
  "<backtab>" 'indent-left
  "c" 'clone-region
  "x" 'exchange-point-and-mark)

(selected-global-mode)

;;

(setq-default
 fill-column 75
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %b:%l:%c"))

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
    ;; FIXME
    ;; a better solution than patching values from other places
    (let ((process-environment
           (cons "PAGER=less" process-environment)))
      (start-process "term" nil terminal-prog)))
  (global-set-key (kbd "C-x t") 'term-here))


;;
;; Horizontal scrolling
;;

(defun small-scroll-right () (interactive) (scroll-right 5 t))
(defun small-scroll-left () (interactive) (scroll-left 5 t))
(gsk "<C-prior>" 'small-scroll-right)
(gsk "<C-next>" 'small-scroll-left)

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
  (strip-ansi-current-buffer)
  (delete-trailing-whitespace))

;;

(add-hook 'text-mode-hook 'goto-address-mode)

;;

(defun maybe-visual-line-mode ()
  "Look at the first 10 lines of the current buffer. If any
  are longer than 80 chars, turn on visual-line-mode."
  (let ((lines-to-consider 10)
        (trigger-length 80)
        (found-long-line nil))
    (while (and (<= (line-number-at-pos) lines-to-consider)
                (not found-long-line))
      (setq found-long-line
            (> trigger-length (- (point-at-eol) (point-at-bol))))
      (forward-line))
    (if found-long-line
        (visual-line-mode t))))

(add-hook 'markdown-mode-hook 'maybe-visual-line-mode)
(add-hook 'text-mode-hook 'maybe-visual-line-mode)

;;

(setq scroll-preserve-screen-position t)

;;

(defun pp-json (beg end)
  (interactive "r")
  (shell-command-on-region beg end "jq" nil t))

;; todo dupe-to-other-window
;; todo rust format on save
;; todo template expand S-M-tab ?

;;

(require 'help-mode)

(defun revert-help-no-confirm ()
  (interactive)
  (help-mode-revert-buffer nil t))

(define-key help-mode-map (kbd "g") 'revert-help-no-confirm)

;;

(require 'swiper)

(defun swiper-selection ()
  (interactive)
  (let ((str (buffer-substring-no-properties (point) (mark))))
    (deactivate-mark)
    (swiper str)))

(gsk "C-'" 'swiper-selection)

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

(gsk "<kp-decimal>" 'highlight-symbol-at-point)

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

;; todo - could handle README.txt as well

;;

;; (when (require 'csv-mode nil t)
;;   (add-hook 'csv-mode-hook 'csv-align-mode))
