(defun name-note-file-now (title ext)
  (file-name-concat
   note-root
   (concat (format-time-string "%Y%m%d.%H%M%S")
           (if (string-empty-p title) "" (concat "." title))
           (if ext (concat "." ext) ""))))

(defun take-notes (title)
  (interactive "M")
  (let ((inferred-ext (when (string-match "\\.\\([[:alpha:]]+\\)" title)
                        (match-string 1 title))))
    (find-file (name-note-file-now title (if inferred-ext nil "txt")))))

(defun jump-to-notes-dir ()
  (interactive)
  (find-file note-root)
  (end-of-buffer))

(defun find-active-note ()
    (interactive)
    find-file
    (file-name-concat note-root "active.txt"))

(defun notes-import-file (file name)
  (interactive "f\nM")
  (copy-file file (name-note-file-now name (file-name-extension file))))

(defalias 'nn 'take-notes)
(defalias 'jn 'jump-to-notes-dir)

(provide 'notes)
