(defun insert-date ()
  (interactive)
  (insert
   (format-time-string
    "%Y-%m-%d")))

(defun insert-date-time ()
  (interactive)
  (insert
   (format-time-string
    "%Y-%m-%d %H:%M")))

(defun insert-date-time-ruler ()
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
