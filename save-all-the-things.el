;;; save-all-the-things.el --- Save all the things mode.

;;; Commentary:

;; Auto save buffers while you're not doing anything.

;; A buffer is automatically saved a period of time after the user has
;; been inactive, even if the buffer is no longer the current buffer.

;;; Code:

(defvar-local save-all-the-things--timer nil
  "Timer for each buffer to automatically save itself.")

(defvar-local save-all-the-things--state 'frustrated
  "Indicator of the current save state.")

(defvar save-all-the-things-delay 1.0
  "Time to wait after a change before saving (seconds).")

(defun save-all-the-things--timer-setter ()
  "Reset any existing save timer and set a new one."
  (when save-all-the-things--timer
    (cancel-timer save-all-the-things--timer))
  (setq-local save-all-the-things--timer
              (run-at-time save-all-the-things-delay nil
                           'save-all-the-things--saver
                           (current-buffer))))

(defun save-all-the-things--saver (buffer)
  "Save BUFFER, if it still exists, and do it quietly."
  (when buffer
    (with-current-buffer buffer
      (cond
       ((not (buffer-file-name)) ;; Buffer has no associated file.
        (setq-local save-all-the-things--state 'frustrated))
       ((not (file-regular-p (buffer-file-name))) ;; Buffer is weird?
        (setq-local save-all-the-things--state 'frustrated))
       ((not (buffer-modified-p)) ;; No changes.
        (setq-local save-all-the-things--state 'all-sweet))
       ((not (verify-visited-file-modtime))
        (message "File has been changed outside Emacs, save-all-the-things will not do its thing.")
        (setq-local save-all-the-things--state 'badly-frustrated))
       (t
        (let ((inhibit-message t)) (save-buffer))
        (setq-local save-all-the-things--state 'all-sweet))))
    (force-mode-line-update)))

(defun save-all-the-things--after-change (x y z)
  "Indicate that the buffer contents have changed."
  (setq-local save-all-the-things--state 'pending))

(defun save-all-the-things-mode-line-indicator ()
  "A mode-line element function to provide state indication.
Add it to your MODE-LINE-FORMAT list like so:
    (:eval (save-all-the-things-mode-line-indicator))"
   (if save-all-the-things-mode
      (pcase save-all-the-things--state
        ('pending ":|")
        ('all-sweet ":)")
        ('frustrated '(:propertize ":S" face compilation-warning))
        ('badly-frustrated '(:propertize ":( :( :(" face compilation-error))
        (other "bad state"))
    '(:propertize "!!!" face compilation-error) ;; save-all-the-things is not enabled!
    ))

(define-minor-mode save-all-the-things-mode
  "Automatically save the buffer after there has been no input for a while."
  :lighter " satt"
  :global t
  (if save-all-the-things-mode
      (progn (add-hook 'post-command-hook 'save-all-the-things--timer-setter)
             (add-hook 'after-change-functions 'save-all-the-things--after-change nil t))
    (remove-hook 'post-command-hook 'save-all-the-things--timer-setter)
    (remove-hook 'after-change-functions 'save-all-the-things--after-change t)))

(provide 'save-all-the-things)

;;; save-all-the-things.el ends here
