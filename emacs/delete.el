;; todo a minor mode that doesn't clobber paredit

;;
;; Stop killing text. Just delete it.
;;

(defun delete-whole-line (n)
  (interactive "p")
  (delete-region
   (line-beginning-position)
   (min (point-max) (+ 1 (line-end-position n)))))

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

(define-minor-mode text-deletion-mode
  "Simple re-bindings to avoid saving to kill-ring for common deletions."
  :global t
  :keymap
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<M-backspace>") 'delete-backward-word)
    (define-key km (kbd "<C-backspace>") 'delete-backward-word)
    (define-key km (kbd "M-d") 'delete-forward-word)
    (define-key km (kbd "<C-delete>") 'delete-forward-word)
    (define-key km (kbd "<C-S-backspace>") 'delete-whole-line)
   km))

