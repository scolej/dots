(defvar-local manual-marks '())

(defun manual-mark-toggle ()
  (interactive)
  (let ((p (point-marker)))
    (if (seq-contains-p manual-marks p (lambda (a b) (eq (marker-position a) (marker-position b))))
        (progn
          (setq manual-marks (delete p manual-marks))
          (message "marker removed"))
      (progn
        (add-to-list 'manual-marks p)
        (sort manual-marks '<)
        (message "marker added")))))

(defun manual-mark-next ()
  (interactive)
  (let* ((p (point))
         (target (car (seq-filter (lambda (x) (> (marker-position x) p)) manual-marks))))
    (goto-char (marker-position target))))

(defun manual-mark-prev ()
  (interactive)
  (let* ((p (point))
         (target (car (seq-filter (lambda (x) (< (marker-position x) p)) (reverse manual-marks)))))
    (goto-char (marker-position target))))

(gsk "C-/" 'manual-mark-toggle)
(gsk "C-." 'manual-mark-next)
(gsk "C-," 'manual-mark-prev)
