(add-hook 'occur-hook 'occur-rename-buffer)

(define-key occur-mode-map (kbd "n")
  (lambda () (interactive)
    (occur-next)
    (occur-mode-display-occurrence)))

(define-key occur-mode-map (kbd "p")
  (lambda () (interactive)
    (occur-prev)
    (occur-mode-display-occurrence)))

(defun occur-selection ()
  (interactive)
  (let ((str (buffer-substring-no-properties (point) (mark))))
    (deactivate-mark)
    (occur str)))

;; todo
;;
;; how to make window only as big as entries?
