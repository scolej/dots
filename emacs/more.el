;;
;; TODO & ideas
;;

;; Better jumping to file paths under point.

;; Long mouse presses for word & line selection

;; C-x C-e for lisp should eval-region if region is active.

;;
;;
;;

(require 'dired-x)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(setq mouse-1-click-follows-link 450)
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file)
;; (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file-other-window)
(global-set-key (kbd "<escape>") 'dired-jump)
(setq dired-guess-shell-alist-default '(("\\.mp4\\'" "vlc")))

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-q") 'quit-window)

(setq-default
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face success)
            "%*"))
   " %b:%l:%c"))

(setq-default show-trailing-whitespace t)

(setq split-width-threshold nil)

(setq delete-selection-save-to-register "d")
(global-set-key (kbd "M-v") 'delete-selection-repeat-replace-region)

(global-set-key (kbd "<f1>") 'switch-to-buffer)

;;
;; Auto saving
;;

(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)

;;
;; Stop killing text. Just delete it.
;;

(defun delete-whole-line ()
  (interactive)
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))
(global-set-key (kbd "<C-S-backspace>") 'delete-whole-line)

(defun delete-forward-word ()
  (interactive)
  (delete-region (point) (save-excursion (forward-word) (point))))
(global-set-key (kbd "M-d") 'delete-forward-word)

(defun delete-backward-word ()
  (interactive)
  (delete-region (point) (save-excursion (backward-word) (point))))
(global-set-key (kbd "<M-backspace>") 'delete-backward-word)

(defun delete-forward-line ()
  (interactive)
  (delete-region (point) (line-end-position)))
(global-set-key (kbd "C-k") 'delete-forward-line)

(defun delete-backward-line ()
  (interactive)
  (delete-region (point) (line-beginning-position)))
(global-set-key (kbd "<C-backspace>") 'delete-backward-line)

(defun delete-forward-sexp ()
  (interactive)
  (delete-region (point) (save-excursion (forward-sexp) (point))))
(global-set-key (kbd "C-M-k") 'delete-forward-sexp)

(defun delete-backward-sexp ()
  (interactive)
  (delete-region (point) (save-excursion (backward-sexp) (point))))
(global-set-key (kbd "<C-M-backspace>") 'delete-backward-sexp)

;;
;; Windowing
;;

(global-set-key (kbd "C-0") #'delete-window)
(global-set-key (kbd "C-1") #'delete-other-windows)
(global-set-key (kbd "C-2") #'split-window-vertically)
(global-set-key (kbd "C-3") #'split-window-horizontally)

;; (global-set-key (kbd "<tab>") #'other-window)
;; (global-set-key (kbd "<S-tab>") (lambda () (interactive) (other-window -1)))

;; To fight the global definition above.
;; FIXME Global minor mode probably better?
(define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)

(global-set-key (kbd "C-`") #'make-frame)

;;
;; Longmouse
;;
;; Functions and bindings for long-pressing right mouse button for copy / cut.
;;

(defvar longmouse-timer nil)

(defun longmouse-down ()
  (interactive)
  (setq longmouse-timer
        (run-at-time 0.3
                     nil
                     '(lambda ()
                        (setq mouse-timer nil)
                        (kill-region (point) (mark))))))
(defun longmouse-up ()
  (interactive)
  (unless (eq longmouse-timer nil)
    (progn
      (kill-ring-save nil nil t)
      (setq deactivate-mark nil)
      (message "Saved region.")
      (cancel-timer longmouse-timer)
      (setq longmouse-timer nil))))

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

(defun google (term)
  (interactive "MGoogle: ")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))

(global-set-key (kbd "C-c g") (call-maybe-with-region 'google))

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
;; Load a persistent scratch on startup.
;;

(when (boundp 'persistent-scratch-file)
  (find-file (symbol-value 'persistent-scratch-file))
  (end-of-buffer))

;;
;; Other packages
;;

(require 'ivy)
(ivy-mode 1)

(require 'projectile)
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
(global-set-key (kbd "<f2>") 'projectile-find-file)
(setq projectile-indexing-method 'alien
      projectile-completion-system 'default)

(require 'ag)