(defun dedicate-window ()
  (interactive)
  (set-window-dedicated-p nil t))

(defun undedicate-window ()
  (interactive)
  (set-window-dedicated-p nil nil))

(defun rando-string ()
  (interactive)
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890![]{}()")
        (len 15)
        (p ""))
    (while (< (length p) len)
      (let ((random-char (string (elt chars (random (length chars))))))
        (setf p (string-join (list p random-char)))))
    (insert p)))

(defun google (term)
  (interactive "M")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))

(defun chunky-scroll-left () (interactive) (scroll-left 20))
(defun chunky-scroll-right () (interactive) (scroll-right 20))
(defun small-scroll-left () (interactive) (scroll-left 10))
(defun small-scroll-right () (interactive) (scroll-right 10))

(defun delete-other-frames ()
  (interactive)
  (mapc #'(lambda (f)
            (unless (eq f (selected-frame))
              (delete-frame f)))
        (frame-list)))


(defun please-help-me ()
  (interactive)
  (let ((s (intern-soft (thing-at-point 'symbol t))))
    (cond ((null s) (message "Nothing :("))
          ((fboundp s) (describe-function s))
          ((boundp s) (describe-variable s))
          (t (message "Don't know what the thing is :(")))))

(defun try-find-file ()
  (interactive)
  (let* (
         (str (or (when (and (use-region-p)
                             (not (minibufferp)))
                    (let ((region (buffer-substring-no-properties (point) (mark))))
                      (if (file-exists-p region) region nil)))
                  (ffap-file-at-point)
                  (error "Not a file :("))))
    (find-file str)))

(defun really-kill-buffer ()
  (interactive) (kill-buffer nil))

(defun set-default-directory (d)
  (interactive "D")
  (setq-local default-directory d))

(defun switch-theme (theme)
  "Disable all other themes and enable THEME."
  (interactive
   (list (intern (completing-read "Switch to custom theme: "
			                      (mapcar 'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme))

(defun words-dammit ()
  "I just want word wrapping!"
  (interactive)
  (fundamental-mode)
  (toggle-truncate-lines 0)
  (visual-line-mode t))

(defun find-file-with-region (start end)
  (interactive "r")
  (let ((f (buffer-substring-no-properties start end)))
    (if (file-exists-p f)
        (find-file f)
      (message "Region does not name a file."))))

(defun copy-buffer-path ()
  "Copy the full path to the current buffer's file."
  (interactive)
  (let ((str (cond (buffer-file-name)
                   (default-directory))))
    (kill-new str)
    (message (format "Copied %s" str))))

(defun copy-buffer-path-and-line ()
  "Copy the full path to the current buffer's file and append a
colon followed by the line number."
  (interactive)
  (let ((s (concat (buffer-file-name)
                   ":"
                   (number-to-string (line-number-at-pos (point))))))
    (kill-new s)
    (message (format "Copied %s" s))))

(defun pop-region ()
  "Take the active region to a new buffer."
  (interactive)
  (when (region-active-p)
    (let ((b1 (current-buffer))
          (b2 (generate-new-buffer "pop")))
      (with-current-buffer b2
        (insert (with-current-buffer b1
                  (buffer-substring-no-properties (point) (mark)))))
      (switch-to-buffer b2))))

(defun pop-region-to-new-file (new-file)
  (interactive "G")
  (pop-region)
  (write-file new-file))
