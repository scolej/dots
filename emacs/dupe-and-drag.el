;;
;; Duplicating
;;

(defvar-local duplication-text nil
  "Contents that we're currently duplicating / last duplicated.")

(defun duplicate-region (dir)
  (interactive)
  (let* ((p (point))
         (m (mark))
         (text (buffer-substring-no-properties p m))
         (deactivate-mark nil))
    (setq duplication-text text)
    (deactivate-mark)
    (duplicate dir)))

(defun duplicate (dir)
  (unless duplication-text
    (error "No duplication text"))
  (cond
   ((or t (eq dir 'up))
    (insert duplication-text))
   ((eq dir 'down)
    (insert-before-markers duplication-text))))

(defun duplicate-line (dir)
  (interactive)
  (let* ((bol (point-at-bol))
         (eol (point-at-eol))
         (text (buffer-substring-no-properties bol (1+ eol))))
    (setq duplication-text text)
    (deactivate-mark)
    (cond ((eq dir 'down) (forward-line))
          ((eq dir 'up) (beginning-of-line)))
    (duplicate dir)))

(defun duplicate-dwim-1 (dir)
  (interactive)
  (message "%s" last-command)
  (save-excursion
    (cond
    ((region-active-p)
     (duplicate-region dir))
    ((or (eq last-command 'duplicate-down)
         (eq last-command 'duplicate-up))
     (duplicate dir))
    (t
     (duplicate-line dir)))))

(defun duplicate-up () (interactive) (duplicate-dwim-1 'up))
(defun duplicate-down () (interactive) (duplicate-dwim-1 'down))

(global-set-key (kbd "<C-M-up>") 'duplicate-up)
(global-set-key (kbd "<C-M-down>") 'duplicate-down)

;;
;; Dragging
;;

(defun drag (dir)
  (interactive)
  (unless (region-active-p)
    (let* ((bol (point-at-bol))
           (eol (point-at-eol))
           (pos-on-line (- (point) bol))
           (text (buffer-substring-no-properties bol eol)))
      (delete-region bol (progn (forward-line) (point)))
      (cond ((eq dir 'down) (end-of-line) (newline))
            ((eq dir 'up) (forward-line -1) (save-excursion (newline))))
      (save-excursion (insert text))
      (forward-char pos-on-line))))

(defun drag-up () (interactive) (drag 'up))
(defun drag-down () (interactive) (drag 'down))

(global-set-key (kbd "<M-down>") 'drag-down)
(global-set-key (kbd "<M-up>") 'drag-up)
