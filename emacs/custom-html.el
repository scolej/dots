;; Replace garbage built-in para function.
(defun html-paragraph ()
  (interactive)
  (let ((t0 "<p>")
        (t1 "</p>"))
    (if (region-active-p)
        (progn
          (save-excursion
            (let ((p0 (region-beginning))
                  (p1 (region-end)))
              (goto-char p1)
              (insert t1)
              (goto-char p0)
              (insert t0))))
      (insert "<p>")
      (save-excursion
        (insert "</p>"))
      (indent-for-tab-command))))
