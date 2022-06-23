(defun eval-line ()
  (interactive)
  (save-excursion
    (let ((exp (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
      (other-window 1)
      (insert exp)
      (comint-send-input)
      (other-window 1))))

;; (defun eval-line-and-insert ()
;;   (interactive)
;;   (save-excursion
;;     (let ((exp (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
;;       (other-window 1)
;;       (insert exp)
;;       (comint-send-input)
;;       (comint-w)
;;       (let ((res
;;              ;; (buffer-substring-no-properties (point-at-bol 0)
;;              ;;                                     (point-at-eol 0))
;;              (buffer-substring-no-properties (point-at-bol) (point-at-eol))
;;              ))
;;         (other-window 1)
;;         (end-of-line)
;;         (newline)
;;         (insert "= " res)))))

(gsk "<f6>" 'eval-line)
