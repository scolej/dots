;;
;; SMERGE!
;;

(setq smerge-command-prefix (kbd "C-c v"))

(defun smerge-maybe ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode))))

(add-hook 'buffer-list-update-hook 'smerge-maybe)
