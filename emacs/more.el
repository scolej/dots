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

(setq mouse-1-click-follows-link 450)
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-display-file)
(define-key dired-mode-map (kbd "o") 'dired-display-file)

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

(fringe-mode 0)

(setq-default truncate-lines t)

(global-set-key (kbd "C-x C-d") nil)

;; (global-set-key (kbd "C-g") 'top-level)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-\\") 'replace-string)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(setq split-width-threshold nil
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

;; FIXME doesn't work at start of buffer
(defun new-line-above ()
  (interactive)
  (forward-line -1)
  (end-of-line)
  (newline nil t))

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
(global-set-key (kbd "C-`") #'make-frame)

;;
;; Longmouse
;;
;; Functions and bindings for long-pressing right mouse button for copy / cut.
;;

(defvar longmouse-timer nil)

(defun longmouse-down ()
  (interactive)
  (kill-new (buffer-substring-no-properties (point) (mark)))
  (setq deactivate-mark t
        longmouse-timer-1 (run-at-time 0.3 nil
                                       (lambda ()
                                         (message "Cut!")
                                         (delete-region (point) (mark))))
        ;; longmouse-timer-2 (run-at-time 0.9 nil
        ;;                                (lambda ()
        ;;                                  (message "Buried!")
        ;;                                  ;; (pop kill-ring)
        ;;                                  ;; (setq kill-ring-yank-pointer kill-ring)
        ;;                                  (setq kill-ring-yank-pointer (cdr kill-ring))
        ;;                                  ))
        ))

(defun longmouse-up ()
  (interactive)
  (dolist (timer (list longmouse-timer-1
                       ;; longmouse-timer-2
                       ))
    (when timer (cancel-timer timer))
    (setq timer nil)))

(global-set-key [down-mouse-3] 'longmouse-down)
(global-set-key [mouse-3] 'longmouse-up)
(global-set-key [mouse-2] 'yank)

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
;; Org
;;

(require 'org)

(setq org-refile-use-outline-path 'file
      org-export-with-section-numbers nil)

(setq org-fontify-done-headline t)
(let ((c "#b0d483"))
 (set-face-attribute 'org-headline-done nil :foreground c)
  (set-face-attribute 'org-done nil :foreground c))

(global-set-key (kbd "C-x c") 'org-capture)

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

;; (defun long-line-truncator (str)
;;   (let ((l 120))
;;     (if (<= (length str) l)
;;         str
;;       (let ((start (substring str 0 l))
;;             (end (substring str l)))
;;         (concat start "\n" (long-line-truncator end))))))

;; (defun long-line-truncator (str)
;;   (replace-regexp-in-string "^\\(.\\{80\\}\\).*?$" ))

;; (add-hook 'comint-preoutput-filter-functions 'long-line-truncator)

;;

;; (defun mark-and-forward ()
;;   (interactive)
;;   (unless (region-active-p) (set-mark (point)))
;;   (forward-sexp))

;; (global-set-key (kbd "<tab>") 'mark-and-forward)

(global-set-key (kbd "<escape>") 'execute-extended-command)

(global-set-key (kbd "<f3>") 'jump-to-register)
