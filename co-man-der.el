;; TODO Some way to know which jobs are running and kill them
;; TODO Kill all buffers which are ouput buffers.
;; TODO Don't show new buffer until output occurs?
;; TODO Could fade commands by frequency of use, and then have a cull function which removes infrequent ones.

(require 'subr-x)

(defun backward-find-default-dir ()
  "Get the last line which started with a non-blank character."
  (save-excursion
    (re-search-backward "^[^[:space:]].*$")
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun current-line ()
  (string-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun char-is-whitespace (c)
  (= 32 (char-syntax c)))

(defun shell-mouse-line (e)
  "Move cursor to click position and execute the command on that line."
  (interactive "e")
  (mouse-set-point e)
  (shell-this-line-in-dir-context))

(defun shell-this-line-in-dir-context ()
  (interactive)
  (if (not (char-is-whitespace (char-after (point-at-bol))))
      (message "Line is not a command! (Needs to be indented.)")
    (progn
      (let* ((command (current-line))
             (directory (backward-find-default-dir))
             (buf (get-buffer-create (string-join (list "moss:" directory command) " "))))
        (unless (string-empty-p command)
          (do-a-command buf command directory)
          (display-buffer buf))))))

(defun co-man-der-kill-process ()
  (interactive)
  (kill-process (get-buffer-process (current-buffer))))

(defun do-a-command (buf command directory)
  (with-current-buffer buf
    (setq-local default-directory directory)
    (async-shell-command command (current-buffer))
    (co-man-der-view-mode t)
    (setq-local show-trailing-whitespace nil)
    (setq-local co-man-der-dir directory)
    (setq-local co-man-der-command command)))

(defun co-man-der-maybe-refresh ()
  "Re-run the command which was used to generate the contents of this buffer."
  ;; FIXME Check if still running??
  (interactive)
  (when (get-buffer-process (current-buffer)) (error "Process is already running."))
  (if (and (boundp 'co-man-der-command) (boundp 'co-man-der-dir))
      (let ((original-point (point)))
        (deactivate-mark)
        (do-a-command (current-buffer) co-man-der-command co-man-der-dir))
    ;; (goto-char original-point)) ;; Need to wait till after
    (message "No command to re-run.")))

;; FIXME Is this really useful?
(defun use-selection-for-new-command (start end)
  (interactive "r")
  (when (region-active-p)
    (let ((text (buffer-substring-no-properties start end)))
      (pop-to-buffer "commands.moss") ;; FIXME No guarantee for this name.
      (co-man-new-command)
      (save-excursion (insert text)))))

(defun append-selection-at-point (start end)
  (interactive "r")
  (when (region-active-p)
    (let* ((text (buffer-substring-no-properties start end))
           (need-quotes? (s-contains? " " text)))
      (with-current-buffer "commands.moss" ;; FIXME No guarantee for this name.
        (move-end-of-line nil)
        (unless (equal (char-before) ?\s) (insert " "))
        (when need-quotes? (insert "\""))
        (insert text) ;; FIXME Point doesn't move?
        (when need-quotes? (insert "\""))))))

(defun co-man-new-command ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(defvar moss-speedy-buffer nil)

(defun moss-this-is-the-speedy-buffer ()
  (interactive)
  (setq moss-speedy-buffer (current-buffer)))

(defun moss-speedy-rerun ()
  (interactive)
  (if moss-speedy-buffer
      (with-current-buffer moss-speedy-buffer
        (co-man-der-maybe-refresh))
    (message "No speedy buffer chosen.")))

(global-set-key (kbd "<kp-enter>") #'moss-speedy-rerun)

(defvar co-man-der-view-mode-map (make-sparse-keymap))
;; (define-key co-man-der-view-mode-map (kbd "q") 'delete-window)
(define-key co-man-der-view-mode-map (kbd "q") 'quit-window)
(define-key co-man-der-view-mode-map (kbd "g") 'co-man-der-maybe-refresh)
(define-key co-man-der-view-mode-map (kbd "d") 'co-man-der-kill-process)
(define-key co-man-der-view-mode-map (kbd "u") 'use-selection-for-new-command)
(define-key co-man-der-view-mode-map (kbd "a") 'append-selection-at-point)

;; Provide key to kill process
(define-minor-mode co-man-der-view-mode
  "Minor mode to add some shortcuts for command views."
  :lighter " cmdv"
  :keymap co-man-der-view-mode-map)

(defvar co-man-der-mode-map (make-sparse-keymap))
(define-key co-man-der-mode-map (kbd "<mouse-3>") 'shell-mouse-line)
(define-key co-man-der-mode-map (kbd "<return>") 'shell-this-line-in-dir-context)
(define-key co-man-der-mode-map (kbd "<S-return>") 'co-man-new-command)

(defvar moss-highlights
  '(
    ("^ *#.*$" . font-lock-comment-face)
    ("^[^[:space:]].*$" . font-lock-keyword-face)
    ))

(defun shellbow-indent ()
  (indent-line-to (pcase (current-indentation)
                    (1 0) (_ 1))))

(define-derived-mode co-man-der-mode fundamental-mode " cmd"
  (setq-local indent-line-function #'shellbow-indent)
  (setq font-lock-defaults '(moss-highlights)))

(defun jump-to-commands ()
  (interactive)
  ;; FIXME It would be nice if this jumped intelligently to a matching
  ;; existing directory in the buffer to.
  (pop-to-buffer "commands.moss"))

(provide 'co-man-der)
