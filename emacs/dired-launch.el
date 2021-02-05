(defvar dired-launch-programs nil)

(defun dired-launch ()
  (interactive)
  (let* ((f (file-truename (dired-file-name-at-point)))
         (prog (alist-get (file-name-extension f) dired-launch-programs nil nil 'equal)))
    (unless prog (error "No program for file: " f))
    (start-process "*dired launch*" (get-buffer-create "*dired launch*")
                   prog f)))

(define-key dired-mode-map (kbd "J") 'dired-launch)

(provide 'dired-launch)
