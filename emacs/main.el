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



;; Wrappers, shortcuts, utilities

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(defun really-kill-buffer ()
  (interactive) (kill-buffer nil))

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

(defun etags-here ()
  (interactive)
  (shell-command
   "find -type f -iname '*.c' -or -iname '*.h' | xargs etags"
   "*etags")
  (visit-tags-table "TAGS"))

(defun kill-buffer-process ()
  (interactive)
  (kill-process (get-buffer-process (current-buffer))))



(load "grep-setup.el")
(load "idle-highlight.el")
(load "trails.el")
(load "delete.el")
(load "dupe-and-drag.el")
(load "notes.el")
(load "schemeing.el")
(load "search-engines.el")
(load "symbol-scan.el")
(load "time-strings.el")

(load "custom-compile.el")
(load "custom-dired.el")
(load "custom-isearch.el")
(load "custom-occur.el")
(load "custom-org.el")

;;
;; Global bindings
;;

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
  "g" 'google
  "b" 'switch-to-buffer
  "s" (keymap "l" 'highlight-lines-matching-regexp
              "r" 'highlight-regexp
              "u" 'unhighlight-regexp)
  "h" (keymap "f" 'describe-function
              "v" 'describe-variable
              "k" 'describe-key
              "m" 'describe-mode
              "i" 'info)
  "<left>" 'previous-buffer
  "<right>" 'next-buffer
  "<escape>" 'top-level
  "n" 'next-error
  "p" 'previous-error
  "o" 'occur))



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

  ;; ideas
  ;; move up down
  ;; clone
  )

(selected-global-mode)



(setq-default
 fill-column 75
 buffer-file-coding-system 'prefer-utf-8-unix
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %b:%l:%c %f"))

(setq
 next-screen-context-lines 15
 hi-lock-auto-select-face t
 Info-isearch-search t
 Info-use-header-line nil)

(mapc
 (lambda (s) (add-to-list 'yank-excluded-properties s))
 '(face font-lock-face))

(mapc
 (lambda (s) (put s 'disabled nil))
 '(narrow-to-page erase-buffer))

;; todo
;; stop pressing tab so much



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
;; Query replace using region
;;

;; fixme how to repeat easily the last replacment?
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

(gsk "C-S-o" 'new-line-above)
(gsk "C-o" 'new-line-below)



;;
;; Browsing back & forth in directory order.
;;

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



(defun end-of-line-and-next ()
  (interactive)
  (when (= (point-at-eol) (point)) (forward-line))
  (end-of-line))

(defun start-of-line-and-prev ()
  (interactive)
  (when (= (point-at-bol) (point)) (forward-line -1))
  (beginning-of-line))

(gsk "C-e" 'end-of-line-and-next)
(gsk "C-a" 'start-of-line-and-prev)
