;;
;; TODO & ideas
;;

;; Better jumping to file paths under point.

;; Long mouse presses for word & line selection

;; C-x C-e for lisp should eval-region if region is active.

;; CUA repeat replace! So good.

;; If tracking packages with submodules, can depend & load from here.

;; Highlight-thing, add & patch so no highlight whitespace only.

;; Quick jump next occurrence of selection, back and forwards.

;;
;; Dired
;;

(require 'dired-x)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(defun dired-find-here (name)
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

(add-hook 'occur-hook
          '(lambda ()
             (setq truncate-lines t)))

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

(setq-default truncate-lines t)

(global-set-key (kbd "C-x C-d") nil)

;; (global-set-key (kbd "C-g") 'top-level)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-\\") 'replace-string)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-/") 'split-line)

(global-set-key (kbd "<kp-5>") 'kill-whole-line)
(global-set-key (kbd "<kp-1>") 'kill-region)
(global-set-key (kbd "<kp-2>") 'maybe-copy-whole-line)
(global-set-key (kbd "<kp-3>") 'yank)

(setq-default
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %b:%l:%c"))

(setq-default show-trailing-whitespace nil)

(setq split-width-threshold 130
      split-height-threshold 40
      yank-handled-properties nil
      disabled-command-function nil)

(global-set-key (kbd "<f1>") 'switch-to-buffer)

(defun maybe-copy-whole-line ()
  (interactive)
  (if mark-active
      (copy-region-as-kill nil nil t)
    (beginning-of-line 2)
    (if (eq last-command 'maybe-copy-whole-line)
        (kill-append
         (buffer-substring-no-properties
          (point)
          (line-beginning-position 0))
         nil)
    (copy-region-as-kill (point) (line-beginning-position 0)))))

(global-set-key (kbd "M-w") 'maybe-copy-whole-line)

(defun new-line-below ()
  (interactive)
  (end-of-line)
  (newline nil t))

(defun new-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))

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

(defun eww-google (term)
  (interactive "MGoogle: ")
  (eww
   (concat "https://google.com/search?query="
           (url-encode-url term))))

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
;; Dragging & duplicating
;;

;; FIXME Need these?
(defun point-line-start () (save-excursion (beginning-of-line) (point)))
(defun point-line-end () (save-excursion (end-of-line) (point)))

;; FIXME And the upwards version as well!
;; FIXME Region should be deactivated, but consecutive invokations should still work.
(defun duplicate-dwim ()
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring-no-properties (point) (mark)))
            (deactivate-mark nil))
        (goto-char (min (point) (mark)))
        (insert text))
    ;; FIXME buffer-substring-no-properties why so long? it's so common...
    (let ((text (buffer-substring-no-properties (point-line-start)
                                                (point-line-end))))
      (save-excursion
        (end-of-line)
        (newline)
        (insert text))
      ;; FIXME Only meant for interactive use?
      (next-line))))

(global-set-key (kbd "<C-M-down>") 'duplicate-dwim)

(defun drag (direction)
  (interactive)
  (unless (region-active-p)
    (let ((pos-on-line (- (point) (point-line-start)))
          (text (buffer-substring-no-properties
                 (point-line-start)
                 (1+ (point-line-end)))))
      (delete-region (point-line-start) (1+ (point-line-end)))
      (forward-line (if (equal direction 'up) -1 1))
      (save-excursion (insert text))
      (forward-char pos-on-line))))

(defun drag-down () (interactive) (drag 'down))
(defun drag-up () (interactive) (drag 'up))

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

(defun scratchy-root ()
  (concat (if (boundp 'scratchy-dir) scratchy-dir
            "~/scratchy/")
          (format-time-string "%Y/%m/%d/")))

(defun scratchy-dired ()
  (interactive)
  (let ((dir (scratchy-root)))
    (make-directory dir t)
    (dired dir)))

(defun scratchy ()
  (interactive)
  (let ((dir (scratchy-root)))
    (make-directory dir t)
    (find-file (concat dir (format-time-string "%H%M")))))

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

(global-set-key (kbd "<escape>") 'execute-extended-command)
(global-set-key (kbd "<f3>") 'jump-to-register)

;;

(require 'ffap)
(defun ffap-git-diff-file (str)
  (substring str 2))
(add-to-list 'ffap-alist '("[ab]/.*" . ffap-git-diff-file))

(defun insert-random-password ()
  (interactive)
  (insert (shell-command-to-string "openssl rand -base64 20")))

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

(add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))
(set-face-attribute 'vertical-border nil :inherit 'fringe :inverse-video t)
(set-face-attribute 'fringe nil :foreground "grey")
(fringe-mode '(0 . 9))