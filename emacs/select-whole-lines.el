;;; select-whole-lines.el --- Handy selection of whole lines from the middle of lines.

;;; Commentary:

;; A minor mode for selecting whole lines. Shift-selection works as
;; normal for horizontal motions, but for up and down (by line and
;; paragraph), point is returned to the beginning of the line before
;; the motion, making one step easier to mark a chunk of entire lines.

;;; Code:

(defvar-local selecting-whole-lines nil "t if the region is active and was activated by this mode.")

(defun choose-motion (selecting-whole-lines direction paragraph)
  "Choose an appropriate motion function.
SELECTING-WHOLE-LINES should be t if we are doing just that.
DIRECTION is 'up or 'down. PARAGRAPH is t if we should jump by
paragraps."
  (let* ((up-motion (cond (paragraph #'backward-paragraph)
                          (selecting-whole-lines (lambda () (interactive) (forward-line -1)))
                          (#'previous-line)))
         (down-motion (cond (paragraph #'forward-paragraph)
                            (selecting-whole-lines #'forward-line)
                            (#'next-line)))
         (motion (if (eq direction 'up) up-motion down-motion)))
    motion))

(defun select-whole-lines (direction &optional paragraph)
  "Move the cursor and set mark to facilitate easy selection of whole lines.
DIRECTION is either 'up or 'down. If PARAGRAPH is t then move by
paragraph instead."
  (if (region-active-p) (funcall (choose-motion selecting-whole-lines direction paragraph))
    (progn
      ;; Time to activate the region and jump to the start of the line.
      ;; Set transient-mark-mode such that it will be disabled just
      ;; like a normal shift selection.
      (setq-local selecting-whole-lines t)
      (setq-local transient-mark-mode (cons 'only (unless (eq transient-mark-mode 'lambda) transient-mark-mode)))
      (if (eq direction 'up)
          (progn (beginning-of-line 2)
                 (set-mark-command nil)
                 (funcall (choose-motion selecting-whole-lines 'up paragraph)))
        (beginning-of-line)
        (set-mark-command nil)
        (funcall (choose-motion selecting-whole-lines 'down paragraph))))))

(defun select-whole-lines-up () (interactive) (select-whole-lines 'up))
(defun select-whole-lines-down () (interactive) (select-whole-lines 'down))
(defun select-whole-lines-up-paragraph () (interactive) (select-whole-lines 'up t))
(defun select-whole-lines-down-paragraph () (interactive) (select-whole-lines 'down t))

(define-minor-mode select-whole-lines-mode
  "Easily select whole lines."
  :lighter " wl"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<S-down>") #'select-whole-lines-down)
            (define-key map (kbd "<S-up>") #'select-whole-lines-up)
            (define-key map (kbd "<C-S-up>") #'select-whole-lines-up-paragraph)
            (define-key map (kbd "<C-S-down>") #'select-whole-lines-down-paragraph)
            map))

(add-hook 'deactivate-mark-hook (lambda () (setq selecting-whole-lines nil)))

(provide 'select-whole-lines)

;;; select-whole-lines.el ends here
