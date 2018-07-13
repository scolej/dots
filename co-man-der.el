;; TODO Some way to know which jobs are running and kill them
;; TODO Kill all buffers which are ouput buffers.

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
  (interactive "e")
  (mouse-set-point e)
  (shell-this-line-in-dir-context))

(defun shell-this-line-in-dir-context ()
  (interactive)
  (if (not (char-is-whitespace (char-after (point-at-bol))))
      (message "Line is not a command! (Needs to be indented.)")
    (progn
      (let* ((cmd-line (current-line)))
        (display-buffer-pop-up-window
         (do-a-command cmd-line (backward-find-default-dir)) nil)))))

(defun do-a-command (command directory)
  (let ((buf (get-buffer-create (string-join (list "moss:" directory command) " "))))
    (with-current-buffer buf
      (setq-local default-directory directory)
      (async-shell-command command (current-buffer))
      (co-man-der-view-mode t)
      (setq-local show-trailing-whitespace nil)
      (setq-local co-man-der-dir directory)
      (setq-local co-man-der-command command))
    buf))

(defun co-man-der-maybe-refresh ()
  ;; FIXME Check if still running??
  (interactive)
  (if (and (boundp 'co-man-der-command) (boundp 'co-man-der-dir))
      (let ((original-point (point)))
        (do-a-command co-man-der-command co-man-der-dir))
    ;; (goto-char original-point)) ;; Need to wait till after
    (message "No command to re-run.")))

(defun co-man-new-command ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(defvar co-man-der-view-mode-map (make-sparse-keymap))
(define-key co-man-der-view-mode-map (kbd "q") 'delete-window)
(define-key co-man-der-view-mode-map (kbd "g") 'co-man-der-maybe-refresh)
;; Provide key to kill process
(define-minor-mode co-man-der-view-mode
  "Minor mode to add some shortcuts for command views."
  :lighter " cmdv"
  :keymap co-man-der-view-mode-map)

(defvar co-man-der-mode-map (make-sparse-keymap))
(define-key co-man-der-mode-map (kbd "<mouse-3>") 'shell-mouse-line)
(define-key co-man-der-mode-map (kbd "<return>") 'shell-this-line-in-dir-context)
(define-key co-man-der-mode-map (kbd "<S-return>") 'co-man-new-command)

(defvar moss-highlights '(("^[^[:space:]].*$" . font-lock-function-name-face)))

(define-derived-mode co-man-der-mode fundamental-mode " cmd"
  (setq font-lock-defaults '(moss-highlights)))

(provide 'co-man-der)
