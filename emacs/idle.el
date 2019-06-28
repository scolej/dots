(defvar idle-highlight-timer nil)
(defvar idle-highlight-string nil)

(defun idle-highlight-clean ()
  "Remove any active highlight, but leave IDLE-HIGHLIGHT-STRING
intact so we can still use it for searching."
  (when idle-highlight-string
    (unhighlight-regexp idle-highlight-string)))

(defun idle-highlight-activate ()
  (add-hook 'post-command-hook 'idle-highlight-set-timer))

(defun idle-highlight-deactivate ()
  (remove-hook 'post-command-hook 'idle-highlight-set-timer)
  (idle-highlight-clean))

(defun idle-highlight-set-timer ()
  (when idle-highlight-timer (cancel-timer idle-highlight-timer))
  (setq idle-highlight-timer
        (run-at-time 0.5 nil 'idle-highlight-region)))

(defun idle-highlight-region ()
  (when mark-active
    (idle-highlight-clean)
    (let ((str (buffer-substring-no-properties (point) (mark))))
      (unless (string-blank-p str)
        (setq idle-highlight-string (regexp-quote str))
        (highlight-regexp idle-highlight-string 'hi-yellow)))))

(defun idle-highlight-find-next ()
  (interactive)
  (when (and idle-highlight-string
             (save-excursion
               (goto-char (max (point) (mark)))
               (re-search-forward idle-highlight-string nil t)))
    (goto-char (match-end 0))
    (set-mark (match-beginning 0))))

(defun idle-highlight-find-prev ()
  (interactive)
  (when (and idle-highlight-string
             (save-excursion
               (goto-char (min (point) (mark)))
               (re-search-backward idle-highlight-string nil t)))
    (goto-char (match-end 0))
    (set-mark (match-beginning 0))))

(global-set-key (kbd "M-n") 'idle-highlight-find-next)
(global-set-key (kbd "M-p") 'idle-highlight-find-prev)

(add-hook 'activate-mark-hook 'idle-highlight-activate)
(add-hook 'deactivate-mark-hook 'idle-highlight-deactivate)
