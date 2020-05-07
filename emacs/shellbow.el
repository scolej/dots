;;
;; Ideas
;;

;; Should remove this sill ">" business and just have a list of
;; commands. New ones appended at the bottom.
;; C-c $ should archive a region of commands, send them to some other buffer which can be C-r searched.
;; Probably change to one directory per buffer?
;; Shellbow shortcut should go to the buffer for a directory which contains default-directory

;; should bind RET in output to same as g to be consistent

;; A good way to search current buffer for a command and then use it as a new command.
;; Like what C-r would do in a shell.

;; S-return in an output buffer should create a new command for that directory

;; Bring back clicking but have commands which can't be clicked
;; ie, push, clean, rm...

;; Window selection, say some commands which should go to smaller windows.
;; ie `git add` which often nothing useful to say.

;; A quick way to "promote" a command up to the saved list

;; Window selection: apply some sorting... should prefer not to select the `git status` window.

;; Would be cool if default-dir changed based on where your cursor was.

;; Need a way to jump from a buffer to the command which produced it
;; so you can easily edit it.

;; What about a list of commands whose output should not be shown -
;; perhaps `add`, `rm` ?

;; Also a list of commands whose output buffer should not tail the
;; output. ie: git status. Sometimes you want tailing, othertimes you
;; want the start of the output.

;; Syntax table so you can go up/forward etc. to parent directory?

;; Pressing enter with ctive region should use only that as the command. So you can "subset" commands easily.

;; A quick way to get back to the shellbow buffer from command output buffer

;; ">" could be on the same line as first command

;;  Would be cool to have "archive" instead of delete. Send old commands to a history file from which you can autocomplete.

;; shellbow-show-active-processes

;; Maybe f11 should find the last visible shellbow comman buffer and run that. Instead of speedy-rerun.
;; or just provide this as: shellbow-speedy-visible-recent

;; Need a way to run command and have it take over current window

;; broken: quotes for "u"

;; default dir should be file location, make it easy to whip up at the site.

;; FIXME
;; enter on ">" line
;; indent on ">" line

;; change buffer name on process exit? can see status from taskbar then.

(require 'subr-x)
(require 's)
(require 'comint)
(require 'cl)

(defvar-local shellbow-command nil "Command used to generate a buffer's contents.")

(defconst shellbow-buffer-prefix "!")

(defun shellbow-bufferp (b)
  (string-prefix-p shellbow-buffer-prefix (buffer-name b)))

(defun shellbow-kill-output-buffers ()
  (interactive)
  (let* ((bufs (seq-filter 'shellbow-bufferp (buffer-list))))
    (mapc 'kill-buffer bufs)
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
  (string-join (list shellbow-buffer-prefix command directory) " "))

(defun shellbow-line-kind ()
  "Find the type of line which point is currently on."
  (let* ((i (current-indentation))
         (first-char (char-after (+ (line-beginning-position) i))))
    (cond ((char-is-whitespace first-char) 'blank)
          ((equal 0 i) 'directory)
          ((equal ?> first-char) 'quick-command-separator)
          ('command))))

(defun shellbow-execute ()
  "Look at the current line and try to do something with it."
  (interactive)
  (let ((k (shellbow-line-kind)))
    (cond ((equal k 'command) (shellbow-execute-line))
          ((equal k 'directory) (dired (shellbow-current-line)))
          (t (error "No action for this line")))))

(defun shellbow-execute-line ()
  (interactive)
  (let ((command (shellbow-current-line)))
    (when (string-empty-p command) (error "Line is empty."))
    (let* ((directory (shellbow-find-dir))
           (buf (get-buffer-create (shellbow-make-name command directory))))
      (display-buffer buf
                      '((shellbow-display-buffer
                         display-buffer-pop-up-window)
                        . ((inhibit-same-window . t))))
      (shellbow-execute-command buf command directory))))

(defun shellbow-execute-line-here ()
  "Execute the current line, and use the current window to display output."
  (interactive)
  (let ((command (shellbow-current-line)))
    (when (string-empty-p command) (error "Line is empty."))
    (let* ((directory (shellbow-find-dir))
           (buf (get-buffer-create (shellbow-make-name command directory))))
      (switch-to-buffer buf)
      (shellbow-execute-command buf command directory)
      (setq shellbow-speedy-buffer buf))))

;; FIXME Window dedicated? Window already displayed somewhere else?
(defun shellbow-preferred-window-p (win)
  "Filter to select window candidates for displaying new buffers."
  (let ((buf (window-buffer win)))
    (not (or (equal (selected-window) win)
             (get-buffer-process buf)
             (with-current-buffer buf
               (equalp major-mode 'shellbow-mode))))))

(defun shellbow-display-buffer (buffer-to-display alist)
  "Try to display buffer most appropriately for shellbow."
  (let ((win (car (seq-sort-by 'window-use-time '< ;; FIXME Maybe opposite sort is more useful?
                               (seq-filter 'shellbow-preferred-window-p
                                           (window-list))))))
    (when win
      (with-selected-window win
        (switch-to-buffer buffer-to-display)))))

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
  ;; FIXME Recency. buffer-display-time
  (car (seq-filter (lambda (b)
                     (with-current-buffer b
                       (equal major-mode 'shellbow-mode)))
                   (buffer-list))))

(defun shellbow-last-window ()
  "Returns the most recently selected window in shellbow mode."
  (car (sort (get-buffer-window-list (shellbow-last-buffer) nil t)
             (lambda (a b) (> (window-use-time a)
                              (window-use-time b))))))

(defun shellbow-region-or-word ()
  (if (use-region-p)
      (buffer-substring-no-properties (point) (mark))
      (thing-at-point 'sexp t)))

;; FIXME This should be smart enough to jump into the right directory context?
;; This is probably the desired behaviour most of the time.
;; But maybe not...
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
    (with-selected-window (shellbow-last-window)
      (move-end-of-line nil)
      (unless (equal (char-before) ?\s) (insert " "))
      (when need-quotes? (insert "\""))
      (insert text)
      (when need-quotes? (insert "\"")))))

(defun shellbox-find-context-bounds ()
  "Find the start and end of this directory context."
  (let* ((pat "^[^[:space:]].*$")
         (end (save-excursion
                (if (re-search-forward pat nil t) (match-beginning 0)
                  (point-max))))
         (start (save-excursion
                  (if (re-search-backward pat nil t) (match-beginning 0)
                    (point-min)))))
    (cons start end)))

(defun shellbow-find-new-command-location ()
  "Place point at the start of locations for new commands."
  (let* ((bounds (shellbox-find-context-bounds))
         (start (car bounds))
         (end (cdr bounds)))
    (goto-char start)
    (unless (re-search-forward "^ >$" end t)
      (goto-char end)
      (re-search-backward "^.*[^[:space:]]+.*$" start t)
      (goto-char (match-end 0))
      (insert "\n >"))))

(defun shellbow-new-command ()
  (interactive)
  (let ((prefix (when (region-active-p) (buffer-substring-no-properties (point) (mark)))))
    (deactivate-mark)
    (shellbow-find-new-command-location)
    (insert "\n ")
    (when prefix (insert prefix))))

(defvar shellbow-speedy-buffer nil
  "Buffer to use to re-run the last command from anywhere. (Like
  RECOMPILE)")

;; FIXME should display the buffer? but needs to handle if it's already visible on another frame.
(defun shellbow-this-is-the-speedy-buffer ()
  "Make the current shellbow output buffer the target for speedy re-runs."
  (interactive)
  (setq shellbow-speedy-buffer (current-buffer))
  (message "This is the speedy buffer!"))

(defun shellbow-speedy-rerun ()
  "If SHELLBOW-SPEEDY-BUFFER is set, jump there and re-run its command."
  (interactive)
  (unless shellbow-speedy-buffer (error "No speedy buffer chosen."))
  (with-current-buffer shellbow-speedy-buffer
    (shellbow-maybe-refresh)))

(defun shellbow-command-buffer-p (buf)
  (with-current-buffer buf
    (and (boundp 'shellbow-view-mode) shellbow-view-mode)))

(defun shellbow-visible-recent ()
  "Returns the most recent shellbow command buffer which is visible."
  (seq-find 'shellbow-command-buffer-p (buffer-list)))

(defvar shellbow-view-mode-map (make-sparse-keymap))
(define-key shellbow-view-mode-map (kbd "q") 'quit-window)
(define-key shellbow-view-mode-map (kbd "g") 'shellbow-maybe-refresh)
(define-key shellbow-view-mode-map (kbd "d") 'shellbow-kill-process)
(define-key shellbow-view-mode-map (kbd "u") 'shellbow-command-from-selection)
(define-key shellbow-view-mode-map (kbd "a") 'shellbow-append-selection-at-point)
(define-key shellbow-view-mode-map (kbd "f") 'find-file-at-point) ;; FIXME use region
(define-key shellbow-view-mode-map (kbd "<SPC>") 'scroll-up-command)
(define-key shellbow-view-mode-map (kbd "<backspace>") 'scroll-down-command)

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
;; (define-key shellbow-mode-map (kbd "<mouse-3>") nil)
(define-key shellbow-mode-map (kbd "<return>") 'shellbow-execute)
(define-key shellbow-mode-map (kbd "C-c C-c") 'shellbow-execute-line-here)
(define-key shellbow-mode-map (kbd "C-m") 'shellbow-execute-line)
(define-key shellbow-mode-map (kbd "<S-return>") 'shellbow-new-command)

(defvar shellbow-highlights
  '(("^ >$" . font-lock-keyword-face)
    ("^ *#.*$" . font-lock-comment-face)
    ("^[^[:space:]].*$" . font-lock-keyword-face)))

(defun shellbow-indent ()
  (indent-line-to (pcase (current-indentation)
                    (1 0) (_ 1))))

(define-derived-mode shellbow-mode fundamental-mode "shellb"
  (setq-local indent-line-function 'shellbow-indent)
  (setq font-lock-defaults '(shellbow-highlights))
  (setq-local completion-at-point-functions '()
              ;; '(comint-completion-at-point)
              ;; '(comint-dynamic-complete-filename comint-replace-by-expanded-filename)
              ;; TODO write a file completion function which uses the directory context
              )
  (set-syntax-table shellbow-syntax-table))

(add-to-list 'auto-mode-alist '("\\.shellbow\\'" . shellbow-mode))

(provide 'shellbow)
