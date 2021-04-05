;;
;; Stop killing text. Just delete it.
;;

(defun delete-whole-line ()
  (interactive)
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun delete-forward-word ()
  (interactive)
  (delete-region (point) (save-excursion (forward-word) (point))))

(defun delete-backward-word ()
  (interactive)
  (delete-region (point) (save-excursion (backward-word) (point))))

(defun delete-forward-line ()
  (interactive)
  (delete-region (point) (line-end-position)))

(defun delete-backward-line ()
  (interactive)
  (delete-region (point) (line-beginning-position)))

(defun delete-forward-sexp ()
  (interactive)
  (delete-region (point) (save-excursion (forward-sexp) (point))))

(defun delete-backward-sexp ()
  (interactive)
  (delete-region (point) (save-excursion (backward-sexp) (point))))

;; (global-set-key (kbd "<C-M-backspace>") 'delete-backward-sexp)
;; (global-set-key (kbd "<C-S-backspace>") 'delete-whole-line)
;; (global-set-key (kbd "<C-backspace>") 'delete-backward-line)
;; (global-set-key (kbd "C-M-k") 'delete-forward-sexp)
;; (global-set-key (kbd "C-k") 'delete-forward-line)

(global-set-key (kbd "<M-backspace>") 'delete-backward-word)
(global-set-key (kbd "M-d") 'delete-forward-word)
