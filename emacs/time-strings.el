(defun insert-time-stamp ()
  (interactive)
  (insert
   (format-time-string
    "%Y-%m-%d %H:%M")))

(defun insert-time-date-ruler ()
  (interactive)
  (insert
   "---------- "
   (format-time-string
    "%Y-%m-%d %H:%M")
   " ----------"))

(defun insert-time-ruler ()
  (interactive)
  (insert
   "----- "
   (format-time-string
    "%H:%M")
   " -----"))
