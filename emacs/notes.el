(defun name-note-file-now (title ext)
  (concat
   note-root
   (format-time-string "%Y%m%d.%H%M%S")
   (if (string-empty-p title) "" (concat "." title))
   "." ext))

(defun take-notes (title)
    ;; (interactive
    ;;  (list
    ;;   (read-from-minibuffer
    ;;    "In file: "
    ;;    (let ((time-string (format-time-string "%Y%m%d.%H%M%S"))
    ;;          (len (string-)))
    ;;      '("**" . 2)))) )
    (interactive "M")
    (find-file (name-note-file-now title "txt")))

(defun jump-to-notes-dir ()
    (interactive)
    (find-file note-root)
    (end-of-buffer))

(defun take-notes-org (title)
    (interactive "M")
    (find-file (name-note-file-now title "org")))

(defun find-active-note ()
    (interactive)
    (find-file (concat note-root "active.txt")))

(defun notes-import-file (file name)
  (interactive "f\nM")
  (copy-file file (name-note-file-now name (file-name-extension file))))
