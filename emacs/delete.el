;;
;; Stop killing text. Just delete it.
;; I dunno who ever thought this was a good idea.
;;

(defun delete-whole-line ()
  (interactive)
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))
(global-set-key (kbd "<C-S-backspace>") 'delete-whole-line)

(defun delete-forward-word ()
  (interactive)
  (delete-region (point) (save-excursion (forward-word) (point))))
(global-set-key (kbd "M-d") 'delete-forward-word)

(defun delete-backward-word ()
  (interactive)
  (delete-region (point) (save-excursion (backward-word) (point))))
(global-set-key (kbd "<M-backspace>") 'delete-backward-word)

(defun delete-forward-line ()
  (interactive)
  (delete-region (point) (line-end-position)))
(global-set-key (kbd "C-k") 'delete-forward-line)

(defun delete-backward-line ()
  (interactive)
  (delete-region (point) (line-beginning-position)))
(global-set-key (kbd "<C-backspace>") 'delete-backward-line)

(defun delete-forward-sexp ()
  (interactive)
  (delete-region (point) (save-excursion (forward-sexp) (point))))
(global-set-key (kbd "C-M-k") 'delete-forward-sexp)

(defun delete-backward-sexp ()
  (interactive)
  (delete-region (point) (save-excursion (backward-sexp) (point))))
(global-set-key (kbd "<C-M-backspace>") 'delete-backward-sexp)
