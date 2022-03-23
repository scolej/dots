;;
;; Scanning back & forth for symbol at point.
;;

(defun scan-for-symbol-at-point (direction)
  (let ((s (thing-at-point 'symbol t))
        (sense (if (eq direction 'forward) 1 -1)))
    (unless s (error "No symbol at point"))
    (save-excursion
      (forward-symbol sense)
      (let ((case-fold-search nil))
        (re-search-forward (concat "\\_<" (regexp-quote s) "\\_>")
                           nil nil
                           sense)))
    (goto-char (match-beginning 0))))

(defun forward-symbol-at-point ()
  (interactive)
  (scan-for-symbol-at-point 'forward))

(defun backward-symbol-at-point ()
  (interactive)
  (scan-for-symbol-at-point 'backward))

(gsk "M-n" 'forward-symbol-at-point)
(gsk "<mouse-3>" 'forward-symbol-at-point)

(gsk "M-p" 'backward-symbol-at-point)
(gsk "<mouse-4>" 'backward-symbol-at-point)

;; Would be better...
;; M-SPC toggles symbol at point as a highlight & candidate for M-n M-p
