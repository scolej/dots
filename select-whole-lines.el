;;; package -- Handy line selection.

;;; Commentary:

;;; Code:

(defvar-local selecting-whole-lines nil)
(defvar-local selecting-whole-lines-restore-column nil)

(defun select-whole-lines (direction)
  "Move the cursor and set mark to facilitate easy selection of whole lines.
DIRECTION is either 'up or 'down."
  (interactive)
  (cond ((and (region-active-p)
              (null selecting-whole-lines))
         (if (eq direction 'up)
             (previous-line)
           (next-line)))
        ((and (region-active-p)
              selecting-whole-lines)
         (if (eq direction 'up)
             (forward-line -1)
           (forward-line)))
        ((null (region-active-p))
         ;; Set transient-mark-mode such that it will be disabled just
         ;; like a normal shift selection.
         (setq-local transient-mark-mode
                     (cons 'only
                           (unless (eq transient-mark-mode 'lambda)
                             transient-mark-mode)))
         (setq selecting-whole-lines t)
         (setq selecting-whole-lines-restore-column (current-column))
         (if (eq direction 'up)
             (progn (beginning-of-line 2)
                    (set-mark-command nil)
                    (forward-line -1))
           (beginning-of-line)
           (set-mark-command nil)
           (forward-line 1)))))

(defun select-whole-lines-up ()
  "Select the current line in preparation for extending the selection upwards."
  (interactive)
  (select-whole-lines 'up))

(defun select-whole-lines-down ()
  "Select the current line in preparation for extending the selection downwards."
  (interactive)
  (select-whole-lines 'down))

(define-minor-mode select-whole-lines-mode
  "Easily select whole lines."
  :lighter " wl"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<S-down>") 'select-whole-lines-down)
            (define-key map (kbd "<S-up>") 'select-whole-lines-up)
            map)
  (add-hook 'deactivate-mark-hook
            (lambda ()
              (when (and selecting-whole-lines
                         selecting-whole-lines-restore-column)
                (move-to-column selecting-whole-lines-restore-column))
              (setq selecting-whole-lines nil))))

;;; select-whole-lines.el ends here
