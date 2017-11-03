;;; save-all-the-things.el --- Save all the things mode.

;;; Commentary:

;; Auto save buffers while you're not doing anything.

;; A buffer is automatically saved a period of time after the user has
;; been inactive, even if the buffer is no longer the current buffer.

;;; Code:

(defvar-local save-all-the-things--timer nil
  "Timer for each buffer to automatically save itsself.")

(defvar save-all-the-things-delay 1
  "Time to wait after a change before saving (seconds).")

(defun save-all-the-things--timer-setter ()
  "Reset any existing save timer and set a new one."
  (when save-all-the-things--timer
    (cancel-timer save-all-the-things--timer))
  (setq-local save-all-the-things--timer
              (run-at-time save-all-the-things-delay nil
                           'save-all-the-things--saver
                           (buffer-name))))

(defun save-all-the-things--saver (buffer-name)
  "Save BUFFER-NAME, if it still exists, and do it quietly."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      (with-current-buffer buf
        (cond
         ((not (buffer-modified-p)) nil) ;; Buffer hasn't changed.
         ((not (verify-visited-file-modtime)) (message "File has been changed outside Emacs, save-all-the-things will not do its thing."))
         ((not (buffer-file-name)) nil) ;; Buffer has no associated file.
         ((not (file-regular-p (buffer-file-name))) nil)
         (t (let ((inhibit-message t)) (save-buffer))))))))

(define-minor-mode save-all-the-things-mode
  "Automatically save the buffer after there has been no input for a while."
  :lighter " satt"
  :global t
  (if save-all-the-things-mode
      (add-hook 'post-command-hook 'save-all-the-things--timer-setter)
    (remove-hook 'post-command-hook 'save-all-the-things--timer-setter)))

(provide 'save-all-the-things)

;;; save-all-the-things.el ends here
