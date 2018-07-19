(defun magical-enter ()
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties (mark) (point))
                 (string-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))
    (cond ((file-exists-p input) (find-file input))
          (t (async-shell-command input)))))

(global-set-key (kbd "<M-RET>") #'magical-enter)
