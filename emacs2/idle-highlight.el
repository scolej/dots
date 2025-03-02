(defvar-local ihi-overlays '())

;; remove all the ihi overlays
(defun ihi-clear ()
  (mapc #'delete-overlay ihi-overlays)
  (setq ihi-overlays '()))

;; create overlays in the visible portion of the window, but only for
;; matches which don't intersect the active region.
(defun ihi-create-overlays (string)
  (let* ((reg (regexp-quote string))
         (win (selected-window))
         (p0 (window-start win))
         (p1 (window-end win))
         (region (region-active-p))
         (r0 (region-beginning))
         (r1 (region-end)))
    (save-excursion
      (goto-char p0)
      (while (re-search-forward reg p1 t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (unless (and region (<= r0 end) (<= beg r1))
            (let ((o (make-overlay beg end)))
              (overlay-put o 'face 'isearch)
              (add-to-list 'ihi-overlays o))))))))

(defun ihi-set (string)
  (ihi-clear)
  (ihi-create-overlays string))

(defvar-local ihi-timer nil)

(defun ihi-reset-timer ()
  (when ihi-timer (cancel-timer ihi-timer))
  (setq ihi-timer (run-at-time 0.2 nil #'ihi-idle)))

(defun ihi-idle ()
  (if (and (region-active-p)
           (not (and (boundp 'rectangle-mark-mode) rectangle-mark-mode))
           (not deactivate-mark))
      (progn (ihi-region) (ihi-reset-timer))
    (ihi-clear)))

(defun ihi-region ()
  (let ((str (string-trim
              (buffer-substring-no-properties
               (region-beginning) (region-end)))))
    (unless (string-empty-p str)
      (ihi-set str))))

;;;

;; (ihi-set "defun")
;; (ihi-clear)
;; (overlay-properties (car ihi-overlays))

(add-hook 'post-command-hook #'ihi-reset-timer)

;; (add-hook 'activate-mark-hook #'ihi-region)
;; (add-hook 'deactivate-mark-hook #'ihi-clear)
