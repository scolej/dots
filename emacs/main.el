;; TODO
;;
;; how to copy stuff, then delete what's left of the line, then paste it somewhere else?
;; first paste is always the blank/garbage you don't want :(

(load "grep-setup.el")
(load "hopper.el")
(load "idle-highlight.el")
(load "trails.el")

;;
;;
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
;;
;;

(defun google (term)
  (interactive "MGoogle: ")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))

;;
;; Global bindings
;;

(require 'pick)
(gsk "<f1>" 'pick-select-buffer)
(gsk "<f2>" 'pick-filelist)
(pick-define-function-keys)
(pick-define-numpad-keys)

(require 'selected)
(define-keys selected-keymap
  "<return>" 'kill-ring-save
  "r" 'query-replace-maybe-region
  "k" 'idle-highlight-keep
  "i" 'indent-rigidly
  ";" 'comment-dwim
  "s" 'sort-lines
  ;; FIXME lose selection after first go :S ???
  ;; ">" 'indent-rigidly-right
  ;; "<" 'indent-rigidly-left
  )
(gsk "<S-return>" 'yank)
;; ?? (gsk "<tab>" 'yank)
(selected-global-mode)

(gsk "<C-tab>" 'other-window)
(gsk "<C-M-backspace>" 'backward-kill-sexp)
(gsk "<M-f4>" 'delete-frame)

(gsk "<M-SPC>" 'cycle-spacing)

(define-keys minibuffer-local-map
  "<escape>" 'top-level
  "<tab>" 'minibuffer-complete)

(gsk "<escape>"
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
      "g" 'google
      "b" 'switch-to-buffer
      "s" 'isearch-forward
      "h" (keymap "f" 'describe-function
                  "v" 'describe-variable
                  "k" 'describe-key
                  "m" 'describe-mode
                  "i" 'info)
      "<left>" 'previous-buffer
      "<right>" 'next-buffer
      "<escape>" 'top-level
      "n" 'next-error
      "p" 'previous-error))

;;
;;
;;

(setq-default fill-column 80
              buffer-file-coding-system 'prefer-utf-8-unix)

(add-to-list 'yank-excluded-properties 'face)
(add-to-list 'yank-excluded-properties 'font-lock-face)

(put 'narrow-to-page 'disabled nil)
(put 'erase-buffer 'disabled nil)

(defun save-all ()
  (interactive)
  (save-some-buffers t))
(gsk "<f12>" 'save-all)

(setq-default
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %b:%l:%c - %f"))

;; todo
;; stop pressing tab so much

(defun really-kill-buffer ()
  (interactive) (kill-buffer nil))

(setq hi-lock-auto-select-face t)

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
(setq isearch-wrap-function '(lambda nil))

;;
;; Dired
;;

(require 'dired-x)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(defun dired-find-here (pattern)
  "Find files in this directory using a wildcard pattern."
  (interactive
   (list
    (read-from-minibuffer
     "Find: " '("**" . 2))) )
  (find-name-dired default-directory pattern))

(defun dired-bro ()
  "Opens the file under point in a browser."
  (interactive)
  (browse-url (dired-filename-at-point)))

(defun dired-launch ()
  (interactive)
  (let* ((f (file-truename (dired-file-name-at-point)))
         (prog (alist-get (file-name-extension f) dired-launch-programs nil nil 'equal)))
    (unless prog (error "No program for file: " f))
    (apply 'start-process "*dired launch*" (get-buffer-create "*dired launch*")
           (funcall prog f))))

(define-keys dired-mode-map
  "<DEL>" 'dired-jump
  "i" 'dired-find-here
  "J" 'dired-launch)

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
       ((eq dir 'up) (goto-char (max p m)) (insert text))
       ((eq dir 'down) (goto-char (min p m)) (insert-before-markers text))))))

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
;;
;;

(when (boundp 'terminal-prog)
  (defun term-here ()
    (interactive)
    (start-process "term" nil terminal-prog))
  (global-set-key (kbd "C-x t") 'term-here))

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
         (regexp-quote str)
         (read-from-minibuffer
          (format "Replace %s with: " str)
          nil nil nil nil str)))
    (call-interactively 'query-replace-regexp)))

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

(global-set-key (kbd "C-S-o") 'new-line-above)
(global-set-key (kbd "C-o") 'new-line-below)

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
;; Paredit & Scheme conf
;;

;; todo
;; - recover M-up/down
;;   more often want to move sexps than do fancy splicing

(require 'paredit)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; todo
;; kind of broken
;; - shadows C-d
;; - shadows M-r
;; - often breaks on pathological output
(add-hook 'inferior-scheme-mode-hook 'paredit-mode)

(define-key paredit-mode-map (kbd "[") 'paredit-open-round)
(define-key paredit-mode-map (kbd "]") 'paredit-close-round)
(define-key paredit-mode-map (kbd "(") 'paredit-open-square)
(define-key paredit-mode-map (kbd ")") 'paredit-close-square)
(define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-round)
;; fixme use multi bind

(defun clone-sexp ()
  (interactive)
  (let* ((b (point))
         (e (save-excursion
              (forward-sexp)
              (point)))
         (s (buffer-substring-no-properties b e)))
    (insert s)
    (newline-and-indent)))

(define-key paredit-mode-map (kbd "M-c") 'clone-sexp)

(put 'match 'scheme-indent-function 1)
(put 'match-let 'scheme-indent-function 1)
(put 'set-fields 'scheme-indent-function 1)

(defun scheme-load-this-file ()
  (interactive)
  (save-buffer)
  (scheme-load-file (buffer-file-name)))

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
;; Browsing back & forth in directory order.
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
;; Scanning back & forth for symbol at point.
;;

(defun scan-for-symbol-at-point (direction)
  (let ((s (thing-at-point 'symbol t))
        (sense (if (eq direction 'forward) 1 -1)))
    (unless s (error "No symbol at point"))
    (save-excursion
      (forward-symbol sense)
      (re-search-forward (concat "\\_<" (regexp-quote s) "\\_>")
                         nil nil
                         sense))
    (goto-char (match-beginning 0))))

(defun forward-symbol-at-point ()
  (interactive)
  (scan-for-symbol-at-point 'forward))
(defun backward-symbol-at-point ()
  (interactive)
  (scan-for-symbol-at-point 'backward))

(global-set-key (kbd "M-n") 'forward-symbol-at-point)
(global-set-key (kbd "M-p") 'backward-symbol-at-point)

;; Would be better... M-SPC toggles symbol at point as a highlight & candidate for M-n M-p

;;
;;
;;

(defun end-of-line-and-next ()
  (interactive)
  (when (= (point-at-eol) (point)) (forward-line))
  (end-of-line))

(defun start-of-line-and-prev ()
  (interactive)
  (when (= (point-at-bol) (point)) (forward-line -1))
  (beginning-of-line))

(global-set-key (kbd "C-e") 'end-of-line-and-next)
(global-set-key (kbd "C-a") 'start-of-line-and-prev)

;;
;;
;;

(when (boundp 'note-root)
  (defun take-notes (title)
    ;; (interactive
    ;;  (list
    ;;   (read-from-minibuffer
    ;;    "In file: "
    ;;    (let ((time-string (format-time-string "%Y%m%d.%H%M%S"))
    ;;          (len (string-)))
    ;;      '("**" . 2)))) )
    (interactive "M")
    (find-file (concat
                note-root
                (format-time-string
                 "%Y%m%d.%H%M%S")
                "." title ".txt"))))

;;
;;
;;

(global-set-key (kbd "<f5>") 'revert-buffer)

;;
;;
;;

(setq Info-isearch-search t
      Info-use-header-line nil)

;;
;; SMERGE!
;;

(setq smerge-command-prefix (kbd "C-c v"))

(defun smerge-maybe ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode))))

(add-hook 'buffer-list-update-hook 'smerge-maybe)

;;
;; Always leave a mark where we last added text.
;;

;; (defun maybe-mark ()
;;   (let ((p (point)))
;;     (if (eq last-command 'self-insert-command)
;;               (set-mark p)
;;             (push-mark p))))

;; (remove-hook 'post-self-insert-hook 'maybe-mark)

;;
;;
;;

(defun etags-here ()
  (interactive)
  (shell-command
   "find -type f -iname '*.c' -or -iname '*.h' | xargs etags"
   "*etags")
  (visit-tags-table "TAGS"))

;;
;;
;;

(defun insert-time-ruler ()
  (interactive)
  (insert
   "--- "
   (format-time-string "%H:%M")
   " ---"))

(defun insert-time-date-ruler ()
  (interactive)
  (insert
   "---------- "
   (format-time-string "%Y-%m-%d %H:%M")
   " ----------"))

;;
;;
;;

(defun kill-buffer-process ()
  (interactive)
  (kill-process (get-buffer-process)))

;;
;;
;;

(gsk "<kp-add>" 'next-error)
(gsk "<kp-subtract>" 'previous-error)

;;
;;
;;

;; (gsk "<M-kp-1>" (lambda () (interactive) (jump-to-register ?1)))
;; (gsk "<S-M-kp-1>" (lambda () (interactive) (point-to-register ?1)))
;; Doesn't work because of some weird key translation?

(defun save-and-recompile ()
  (interactive)
  ;; TODO predicate for same git project files only
  (save-some-buffers t)
  (recompile))

;;
;;
;;

(require 'org)
(define-key org-mode-map (kbd "<tab>") nil)
(define-key org-mode-map (kbd "<C-tab>") nil)
