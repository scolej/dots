(defvar-local ihi-overlays '()
  "List of all the overlays that ihi-mode has created.")


;; remove all the ihi overlays
(defun ihi-clear ()
  (mapc #'delete-overlay ihi-overlays)
  (setq ihi-overlays '()))

;; create overlays in the visible portion of the window, but only for
;; matches which don't intersect the active region.
(defun ihi-create-overlays (string)
  (if (string-empty-p string) (error "can't highlight empty string"))
  (let* ((reg (regexp-quote string))
         (win (selected-window))
         (p0 (window-start win))
         (p1 (window-end win))
         (region (region-active-p))
         (r0 (region-beginning))
         (r1 (region-end)))
    (save-excursion
      (goto-char p0)
      (while (let ((case-fold-search nil)) (re-search-forward reg p1 t))
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (unless (and region (<= r0 end) (<= beg r1))
            (let ((o (make-overlay beg end)))
              (overlay-put o 'face 'isearch)
              (add-to-list 'ihi-overlays o))))))))

(defun ihi-set (string)
  (ihi-clear)
  (ihi-create-overlays string))

(defvar-local ihi-timer nil
  "The currently set ihi-timer, if there is one.")

(defun ihi-reset-timer ()
  (when ihi-timer (cancel-timer ihi-timer))
  (setq ihi-timer (run-at-time 0.1 nil #'ihi-idle)))

(defun ihi-idle ()
  (if (and (region-active-p)
           (not (and (boundp 'rectangle-mark-mode)
                     rectangle-mark-mode)))
      (progn (ihi-region) (ihi-reset-timer))
    (ihi-clear)))

(defun ihi-region ()
  (let* ((beg (region-beginning))
         (end (region-end))
         (str (string-trim (buffer-substring-no-properties beg end))))
    (if (string-empty-p str)
        (ihi-clear)
      (ihi-set str))))

;;;

;; (add-hook 'activate-mark-hook #'ihi-region)
;; (add-hook 'deactivate-mark-hook #'ihi-clear)

(define-minor-mode ihi-mode
  "idle-highlight region mode"
  :global t
  (if ihi-mode
      (add-hook 'post-command-hook #'ihi-reset-timer)
    (remove-hook 'post-command-hook #'ihi-reset-timer)))

