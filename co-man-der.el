;; TODO Highlighting! :)
;; TODO Better buffer name
;; TODO Some way to know which jobs are running and kill them

(defun backward-find-default-dir ()
  "Get the last line which started with a non-blank character."
  (save-excursion
    (re-search-backward "^[^[:space:]].*$")
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun current-line ()
  (string-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun char-is-whitespace (c)
  (= 32 (char-syntax c)))
    
(defun shell-this-line-in-dir-context ()
  (interactive)
  (if (not (char-is-whitespace (char-after (point-at-bol))))
      (message "Line is not a command! (Needs to be indented.)")
    (progn
      (let* ((cmd-line (current-line))
             (default-directory (backward-find-default-dir))
             (out-buffer (get-buffer-create (string-join (list "moss:" default-directory cmd-line) " "))))
        (with-current-buffer out-buffer (read-only-mode -1) (erase-buffer))
        (async-shell-command cmd-line out-buffer)
        (switch-to-buffer-other-window out-buffer)
        (text-scale-set -2)
        (font-lock-mode -1)
        (view-mode)))))

(defvar co-man-der-mode-map (make-sparse-keymap))
(define-key co-man-der-mode-map (kbd "<return>") 'shell-this-line-in-dir-context)
(define-key co-man-der-mode-map (kbd "<S-return>") 'newline)

(defvar moss-highlights '(("^[^[:space:]].*$" . font-lock-function-name-face)))

(define-derived-mode co-man-der-mode fundamental-mode " cmd"
  (setq font-lock-defaults '(moss-highlights)))

(provide 'co-man-der)
