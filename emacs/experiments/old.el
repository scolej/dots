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

(defun dired-bro ()
  "Opens the file under point in a browser."
  (interactive)
  (browse-url (dired-filename-at-point)))

(setq mouse-1-click-follows-link 450)
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-display-file)
(define-key dired-mode-map (kbd "o") 'dired-display-file)
(define-key dired-mode-map (kbd "i") 'dired-find-here)
(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)

(global-set-key (kbd "<S-escape>") 'dired-jump)

(defun dired-launch ()
  (interactive)
  (let* ((f (file-truename (dired-file-name-at-point)))
         (prog (alist-get (file-name-extension f) dired-launch-programs nil nil 'equal)))
    (unless prog (error "No program for file: " f))
    ;; (message "Launch %s for %s" prog f)
    (start-process "*dired launch*" (get-buffer-create "*dired launch*")
                   prog f)
    ;; (pop-to-buffer "*dired launch*")
    ))

(define-key dired-mode-map (kbd "J") 'dired-launch)

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
;;
;;

;; (defun copy-whole-line ()
;;   (interactive)
;;   (beginning-of-line 2)
;;   (if (eq last-command 'maybe-copy-whole-line)
;;       (kill-append
;;        (buffer-substring-no-properties
;;         (point)
;;         (line-beginning-position 0))
;;        nil)
;;     (copy-region-as-kill (point) (line-beginning-position 0))))

;; (global-set-key (kbd "M-w") 'maybe-copy-whole-line)
(global-set-key (kbd "M-w") 'kill-ring-save)

;;
;; Misc
;;

(global-set-key (kbd "C-x C-d") nil)

;; Just use find-file?
(global-set-key (kbd "C-x d") nil)

(global-set-key (kbd "C-\\") 'replace-string)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "<kp-5>") 'kill-whole-line)
(global-set-key (kbd "<kp-1>") 'kill-region)
(global-set-key (kbd "<kp-2>") 'copy-whole-line)
(global-set-key (kbd "<kp-3>") 'yank)

(setq-default
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %b:%l:%c"))

(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil :background "#ffdddd")

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
(global-set-key (kbd "M-r") 'delete-selection-repeat-replace-region)

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
(global-set-key (kbd "<C-tab>") 'other-window)

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
         (read-from-minibuffer (format "Replace %s with: " str) str)))
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
(setq isearch-wrap-function '(lambda nil))


;;
;;
;;

;; (load "idle.el")
;; (load "delete.el")
(load "trails.el")
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

;; (setq completion-styles
;;       '(basic
;;         partial-completion
;;         ;; emacs22
;;         substring
;;         initials))

(setq completion-styles
      '(substring partial-completion basic))

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

(setq smerge-command-prefix (kbd "C-c v"))

(defun smerge-maybe ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode))))

(add-hook 'buffer-list-update-hook 'smerge-maybe)

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

;;
;;
;;

(defvar hopper-root nil)

(defun hopper-cd (dir)
  (interactive "DNew hopper dir: ")
  (setq hopper-root dir))

(defun hop-test-file (dir file)
  (let ((f (concat (file-name-as-directory dir) file)))
    (if (file-exists-p f) f
      nil)))

(defun hop-to-file (file &optional line column)
  (let ((target (or (if (file-exists-p file) file nil)
                    (hop-test-file default-directory file)
                    (hop-test-file hopper-root file))))
    (if (not target) (call-interactively 'find-file)
      (find-file target)
      (when line (goto-line line))
      (when column (move-to-column column))
      (recenter))))

(defconst hopper-not-path-char-regex "[^[:alnum:]-_./:\\]"
  "Regexp matching characters which should mark the bounds of a file path.")

(defun file-hopper ()
  (interactive)
  (let ((str (buffer-substring-no-properties
              (or (when (save-excursion
                          (re-search-backward hopper-not-path-char-regex (point-at-bol) t))
                    (1+ (match-beginning 0)))
                  (point-at-bol))
              (or (when (save-excursion
                          (re-search-forward hopper-not-path-char-regex (point-at-eol) t))
                    (1- (match-end 0)))
                  (point-at-eol)))))
    (cond
     ((or (string-empty-p str)
          ;; (and (null hopper-root)
          ;;      ;; (not (file-name-absolute-p str))
          ;;      )
          )
      (call-interactively 'find-file))
     ((string-match "\\([^:]*\\):\\([[:digit:]]+\\)" str)
      (hop-to-file (match-string 1 str) (string-to-number (match-string 2 str))))
     ;; TODO match line & column
     (t
      (hop-to-file str)))))

(global-set-key (kbd "C-x f") 'file-hopper)
(global-set-key (kbd "C-x C-f") 'find-file)

;;
;;
;;

(defun next-mark ()
  (interactive)
  (let* ((p0 (point))
         (p (point-max)))
    (dolist (m (cons (mark) (mapcar 'marker-position mark-ring)))
      (when (and (> m p0)
                 (< m p))
        (setq p m)))
    (goto-char p)))

(defun prev-mark ()
  (interactive)
  (let* ((p0 (point))
         (p (point-min)))
    (dolist (m (cons (mark) (mapcar 'marker-position mark-ring)))
      (when (and (< m p0)
                 (> m p))
        (setq p m)))
    (goto-char p)))

(defun purge-marks ()
  (interactive)
  (let ((p0 (min (point) (mark)))
        (p1 (max (point) (mark))))
    (setq mark-ring
          (seq-remove
           (lambda (m)
             (let ((mp (marker-position m)))
               (< p0 mp p1)))
           mark-ring))))

(defun purge-all-marks ()
  (interactive)
  (setq mark-ring nil))

;; (global-set-key (kbd "<S-down>") 'next-mark)
;; (global-set-key (kbd "<S-up>") 'prev-mark)

;;
;;
;;

(global-set-key (kbd "<C-tab>") 'other-window)

;;
;;
;;

(defvar-local timer-1 nil)
(defvar-local timer-2 nil)

(defun long-click-init (event)
  (interactive "e")
  (posn-set-point (event-start event))
  (deactivate-mark)
  (setq timer-1 (run-at-time 0.5 nil 'mark-word))
  (setq timer-2 (run-at-time 1.0 nil 'mark-line)))

(defun long-click-cancel ()
  (interactive)
  (when timer-1 (cancel-timer timer-1) (setq timer-1 nil))
  (when timer-2 (cancel-timer timer-2) (setq timer-2 nil)))

(defun mark-word ()
  (interactive)
  (backward-word)
  (push-mark)
  (activate-mark)
  (forward-word))

(defun mark-line ()
  (interactive)
  (beginning-of-line)
  (push-mark)
  (activate-mark)
  (forward-line))

;; (global-set-key (kbd "<mouse-1>") 'mouse-set-point)
;; (global-set-key [down-mouse-1] nil)

;; (global-set-key [down-mouse-1] 'long-click-init)
;; (global-set-key [drag-mouse-1] 'long-click-cancel)
;; (global-set-key [mouse-1] 'long-click-cancel)

;;
;;
;;

;; (global-set-key (kbd "<prior>") 'scroll-down-command)
;; (global-set-key (kbd "<next>") 'scroll-up-command)
(global-set-key (kbd "<prior>") (lambda () (interactive) (forward-line -20)))
(global-set-key (kbd "<next>") (lambda () (interactive) (forward-line 20)))

;;
;; Shell line
;;

(defun shell-this-line ()
  (interactive)
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (end-of-line)
    (newline)
    (insert (shell-command-to-string line))))

(global-set-key (kbd "C-c C-c") 'shell-this-line)

;;
;;
;;

(defun maybe-mark ()
  (unless (eq last-command 'self-insert-command)
    (push-mark (1- (point)))))

(add-hook 'post-self-insert-hook 'maybe-mark)
