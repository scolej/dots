;;; mega-highlight.el --- Interactive highlighting.

;;; Commentary:

;;; Code:

(defvar-local mh--timer nil "The timer for highlighting.")
(defvar mh-highlights nil "User defined highlights to perform.")
(defvar mega-highlight-delay 1 "How long to wait before refresh.")

(defun mh--timer-setter ()
  (when mh--timer (cancel-timer mh--timer))
  (setq-local
   mh--timer
   (run-at-time mega-highlight-delay nil
                'mh--reload-and-refresh
                (buffer-name))))

(defun mh--reload-and-refresh (buffer)
  (eval-buffer buffer)
  (if (null mh-highlights) (message "Define a symbol called mh-highlights")
    (mapc #'highlight-from-scratch (buffer-list))))

(defun do-highlight (pattern)
  "Simple wrapper to highlight PATTERN."
  (highlight-regexp pattern))

(defun highlight-from-scratch (buffer)
  "Clear and refresh the highlights in BUFFER."
  (with-current-buffer buffer
    (when (get-buffer-window nil t) ;; Only operate on visible buffers.
      (unhighlight-regexp t)
      (mapc #'do-highlight mh-highlights))))

(define-derived-mode mega-highlight-mode emacs-lisp-mode " Mh"
  (add-hook 'post-command-hook 'mh--timer-setter nil t))

(provide 'mega-highlight)

;;; mega-highlight.el ends here
