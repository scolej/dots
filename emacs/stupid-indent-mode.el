(defvar stupid-indent-mode-map (make-sparse-keymap))

(define-key stupid-indent-mode-map (kbd "<tab>") 'stupid-indent)
(define-key stupid-indent-mode-map (kbd "<backtab>") 'stupid-deindent)

(defvar-local stupid-indent-indent 4)

(defun point-at-stupid-multiple ()
  (= 0 (mod (- (point) (pos-bol)) stupid-indent-indent)))

(defun char-is-whitespace (c) (and c (or (= c ?\s) (= c ?t))))

(defun stupid-indent ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'stupid-indent-region)
    (stupid-indent-line)))

(defun stupid-deindent ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'stupid-deindent-region)
    (stupid-deindent-line)))

(defun stupid-indent-line ()
  (interactive)
  (back-to-indentation)
  (insert " ")
  (while (not (point-at-stupid-multiple))
    (insert " ")))

(defun stupid-deindent-line ()
  (interactive)
  (back-to-indentation)
  (when (char-is-whitespace (char-before))
    (delete-backward-char 1)
    (while (and (char-is-whitespace (char-before))
                (not (point-at-stupid-multiple)))
      (delete-backward-char 1))))

;; todo what about indent-rigidly-left-to-tab-stop ??

;; todo refactor

;; todo also configure elec indent

(defun stupid-indent-region (beg end)
  (interactive "r")
  (deactivate-mark)
  (let ((line-end (line-number-at-pos end)))
    (save-excursion
      (goto-char beg)
      (while (<= (line-number-at-pos) line-end)
        (stupid-indent-line)
        (forward-line 1))))
  (activate-mark)
  (setq deactivate-mark nil))

(defun stupid-deindent-region (beg end)
  (interactive "r")
  (deactivate-mark)
  (let ((line-end (line-number-at-pos end)))
    (save-excursion
      (goto-char beg)
      (while (<= (line-number-at-pos) line-end)
        (stupid-deindent-line)
        (forward-line 1))))
  (activate-mark)
  (setq deactivate-mark nil))

(define-minor-mode stupid-indent-mode
  "A minor-mode to blow away whatever convoluted indent scheme the
current major-mode has implemented and replace it with the
simplest thing imaginable."
  :keymap stupid-indent-mode-map)

(provide 'stupid-indent-mode)
