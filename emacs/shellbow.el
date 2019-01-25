;;
;; Ideas
;;

;; Would be cool if default-dir changed based on where your cursor was.

;; Need a way to jump from a buffer to the command which produced it
;; so you can easily edit it.

;; What about a list of commands whose output should not be shown -
;; perhaps `add`, `rm` ?

;; Also a list of commands whose output buffer should not tail the
;; output. ie: git status. Sometimes you want tailing, othertimes you
;; want the start of the output.

(require 'subr-x)
(require 's)
(require 'comint)

(defvar-local shellbow-command nil "Command used to generate a buffer's contents.")

(defun shellbow-kill-output-buffers ()
  (interactive)
  (let* ((bufs (seq-filter (lambda (b) (string-prefix-p "*sb* " (buffer-name b)))
                           (buffer-list))))
    (mapc #'kill-buffer bufs)
    (message "Killed %s output buffers" (length bufs))))

(defun shellbow-find-dir ()
  "Search backwards from point for the last line which started
with a non-blank character."
  (save-excursion
    (re-search-backward "^[^[:space:]].*$")
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun shellbow-current-line ()
  "Returns the text on the current line, with leading and
trailing whitespace trimmed."
  (string-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun char-is-whitespace (c)
  (= 32 (char-syntax c)))

(defun shellbow-mouse-line (e)
  "Move cursor to click position and execute the command on that line."
  (interactive "e")
  (mouse-set-point e)
  (shellbow-execute-line))

(defun shellbow-make-name (command directory)
  (string-join (list "*sb*" directory command) " "))

(defun shellbow-execute-line ()
  ;; TODO Maybe this on dir line should dired it.
  (interactive)
  (unless (char-is-whitespace (char-after (point-at-bol)))
    (error "Line is not a command. Commands need to be indented."))
  (let ((command (shellbow-current-line)))
    (when (string-empty-p command) (error "Line is empty."))
    (let* ((directory (shellbow-find-dir))
           (buf (get-buffer-create (shellbow-make-name command directory))))
      (display-buffer buf)
      (shellbow-execute-command buf command directory))))

(defun shellbow-kill-process ()
  "Kill the process associated with the current buffer."
  (interactive)
  (kill-process (get-buffer-process (current-buffer))))

(defun shellbow-execute-command (buffer command directory)
  "Run COMMAND in DIRECTORY with output to BUFFER."
  (with-current-buffer buffer
    (comint-mode)
    (ansi-color-for-comint-mode-on)
    (shellbow-view-mode)
    (setq-local default-directory directory)
    (setq-local shellbow-command command)
    (shellbow-maybe-refresh)))

(defun shellbow-maybe-refresh ()
  "Re-run the command which was used to generate the contents of this buffer."
  (interactive)
  (when (get-buffer-process (current-buffer)) (error "Process is already running."))
  (unless shellbow-command (error "Buffer has no associated command."))
  (deactivate-mark)
  (erase-buffer)
  (set-process-filter
   (start-process-shell-command (shellbow-make-name shellbow-command default-directory)
                                (current-buffer)
                                shellbow-command)
   'comint-output-filter))

(defun shellbow-last-buffer ()
  "Returns the most recently visited buffer in shellbow mode."
  ;; FIXME Recency.
  (car (seq-filter (lambda (b)
                     (with-current-buffer b
                       (equal major-mode 'shellbow-mode)))
                   (buffer-list))))

(defun shellbow-region-or-word ()
  (if (region-active-p)
      (buffer-substring-no-properties (point) (mark))
    (thing-at-point 'sexp t)))

(defun shellbow-command-from-selection (start end)
  "Switch to the most recent shellbow buffer and make a new
command from the current selection or word around point."
  (interactive "r")
  (let ((text (shellbow-region-or-word)))
    (pop-to-buffer (shellbow-last-buffer))
    (shellbow-new-command)
    (save-excursion (insert text))))

(defun shellbow-append-selection-at-point (start end)
  (interactive "r")
  (let* ((text (shellbow-region-or-word))
         (need-quotes? (s-contains? " " text)))
    (with-current-buffer (shellbow-last-buffer)
      (move-end-of-line nil)
      (unless (equal (char-before) ?\s) (insert " "))
      (when need-quotes? (insert "\""))
      (insert text) ;; FIXME Point doesn't move?
      (when need-quotes? (insert "\"")))))

(defun shellbow-new-command ()
  (interactive)
  (let ((prefix (when (region-active-p) (buffer-substring-no-properties (point) (mark)))))
    (deactivate-mark)
    (move-end-of-line nil)
    ;; (end-of-buffer)
    (newline)
    (insert " ")
    (when prefix (insert prefix))))

(defvar shellbow-speedy-buffer nil
  "Buffer to use to re-run the last command from anywhere. (Like
  RECOMPILE)")

(defun shellbow-this-is-the-speedy-buffer ()
  "Make the current shellbow output buffer the target for speedy re-runs."
  (interactive)
  (setq shellbow-speedy-buffer (current-buffer)))

(defun shellbow-speedy-rerun ()
  "If SHELLBOW-SPEEDY-BUFFER is set, jump there and re-run its command."
  (interactive)
  (unless shellbow-speedy-buffer (error "No speedy buffer chosen."))
  (with-current-buffer shellbow-speedy-buffer
    (shellbow-maybe-refresh)))

(defvar shellbow-view-mode-map (make-sparse-keymap))
(define-key shellbow-view-mode-map (kbd "q") 'quit-window)
(define-key shellbow-view-mode-map (kbd "g") 'shellbow-maybe-refresh)
(define-key shellbow-view-mode-map (kbd "d") 'shellbow-kill-process)
(define-key shellbow-view-mode-map (kbd "u") 'shellbow-command-from-selection)
(define-key shellbow-view-mode-map (kbd "a") 'shellbow-append-selection-at-point)

(defvar shellbow-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    table))

(define-minor-mode shellbow-view-mode
  "Minor mode to add some shortcuts for command views."
  :lighter " sbv"
  :keymap shellbow-view-mode-map
  (set-syntax-table shellbow-syntax-table))

(defvar shellbow-mode-map (make-sparse-keymap))
(define-key shellbow-mode-map (kbd "<mouse-3>") 'shellbow-mouse-line)
(define-key shellbow-mode-map (kbd "<return>") 'shellbow-execute-line)
(define-key shellbow-mode-map (kbd "C-m") 'shellbow-execute-line)
(define-key shellbow-mode-map (kbd "<S-return>") 'shellbow-new-command)

(defvar shellbow-highlights
  '(("^ *#.*$" . font-lock-comment-face)
    ("^[^[:space:]].*$" . font-lock-keyword-face)))

(defun shellbow-indent ()
  (indent-line-to (pcase (current-indentation)
                    (1 0) (_ 1))))

(define-derived-mode shellbow-mode fundamental-mode " sb"
  (setq-local indent-line-function #'shellbow-indent)
  (setq font-lock-defaults '(shellbow-highlights))
  (set-syntax-table shellbow-syntax-table))

(add-to-list 'auto-mode-alist '("\\.shellbow\\'" . shellbow-mode))

(provide 'shellbow)
