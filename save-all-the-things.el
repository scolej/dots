;;
;; Experiment with auto saving.
;;
;; A buffer is automatically saved a period of time after the user has
;; been inactive.

(defvar-local save-all-the-things--timer nil
  "Timer for each buffer to automatically save itsself.")

(defvar save-all-the-things-delay 2
  "Time to wait after a change before saving (seconds).")

(defun save-all-the-things--timer-setter ()
  "Reset any existing save timer and set a new one."
  (when buffer-file-name
    (when save-all-the-things--timer
      (cancel-timer save-all-the-things--timer))
    (setq-local save-all-the-things--timer
                (run-at-time save-all-the-things-delay
                             nil
                             'save-all-the-things--saver
                             (buffer-name)))))

(defun save-all-the-things--saver (buf)
  "Save the buffer, if it still exists, and do it quietly."
  (when (get-buffer buf)
    (with-current-buffer buf
      (let ((inhibit-message t))
        (save-buffer)))))

(add-hook 'post-command-hook
          'save-all-the-things--timer-setter)
