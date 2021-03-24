(when (boundp 'note-root)
  (defun take-notes (title)
    ;; (interactive
    ;;  (list
    ;;   (read-from-minibuffer
    ;;    "In file: "
    ;;    (let ((time-string (format-time-string "%Y%m%d.%H%M%S"))
    ;;          (len (string-)))
    ;;      '("**" . 2)))) )
    (interactive "M")
    (find-file (concat
                note-root
                (format-time-string
                 "%Y%m%d.%H%M%S")
                "." title ".txt")))
  (defun find-active-note ()
    (interactive)
    (find-file (concat note-root "active.txt"))))
