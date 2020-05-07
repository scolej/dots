;;
;; Persistent scratchy files, organised in a calendar hierarchy.
;;

(defun scratchy-root ()
  (concat (if (boundp 'scratchy-dir) scratchy-dir
            "~/scratchy/")
          (format-time-string "%Y/%m/%d/")))

(defun scratchy-dired ()
  "Open dired at today's scratch directory."
  (interactive)
  (let ((dir (scratchy-root)))
    (make-directory dir t)
    (dired dir)))

(defun scratchy-ext (ext)
  "Open a new scratch file with extension EXT."
  (interactive)
  (let ((dir (scratchy-root)))
    (make-directory dir t)
    (find-file (concat dir (format-time-string "%H%M") ext))))

(defun scratchy () (interactive) (scratchy-ext ""))
(defun scratchy-elisp () (interactive) (scratchy-ext ".el"))
(defun scratchy-txt () (interactive) (scratchy-ext ".txt"))
