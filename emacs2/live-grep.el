(defun live-grep ()
  (interactive)
  (let ((dir default-directory)
        (pat "")
        (buf (get-buffer-create "*lgrep*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "rg -n -S " pat " " dir "\n")
      (beginning-of-buffer)
      (beginning-of-line)
      (forward-char 9))
    (switch-to-buffer buf)
    (add-hook 'after-change-functions 'live-grep-after-change nil t)))

(defvar live-grep-timer nil)
(defvar live-grep-process nil)

(defun live-grep-after-change (beg end len)
  (let ((p0 (point-min))
        (p1 (save-excursion (beginning-of-buffer) (min (point-at-eol) (point-max)))))
    (when (<= p0 beg end p1)
        (progn
          (when live-grep-timer (cancel-timer live-grep-timer))
          (setq live-grep-timer (run-at-time 0.5 nil 'live-grep-run))))))

;; (defun live-grep-run ()
;;   (let ((proc (get-buffer-process (current-buffer))))
;;     (when (and proc (process-live-p proc))
;;       (kill-process live-grep-process)))
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (forward-line 1)
;;     (delete-region (point) (point-max)))
;;   (let ((cmd (save-excursion
;;                (beginning-of-buffer)
;;                (buffer-substring-no-properties (point-min)
;;                                                (min (point-at-eol) (point-max))))))
;;     (compilation-start cmd 'grep-mode nil t t)))

(defun live-grep-run ()
  (when (and live-grep-process
             (process-live-p live-grep-process))
    (kill-process live-grep-process))
  (save-excursion
    (beginning-of-buffer)
    (forward-line 1)
    (delete-region (point) (point-max)))
  (let ((cmd (save-excursion
               (beginning-of-buffer)
               (buffer-substring-no-properties (point-min)
                                               (min (point-at-eol) (point-max))))))
    (setq live-grep-process
          (start-process-shell-command "lgrep" (current-buffer) cmd))
    (setq-local compilation-filter-hook '(grep-filter t))
    (set-process-filter live-grep-process #'compilation-filter)))
