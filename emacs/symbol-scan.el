;;
;; Scanning back & forth for symbol at point.
;;

;; todo might be handy to have a wrapper which moves to next thing:
;; flymake error, symbol at point, highlighted regex, last isearch?

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

;; todo and if region is active?

(defun backward-symbol-at-point ()
  (interactive)
  (scan-for-symbol-at-point 'backward))

(gsk "M-n" 'forward-symbol-at-point)
(gsk "M-p" 'backward-symbol-at-point)

;; (gsk "<C-down>" 'forward-symbol-at-point)
;; (gsk "<C-up>" 'backward-symbol-at-point)

(gsk "<M-wheel-down>" 'forward-symbol-at-point)
(gsk "<M-wheel-up>" 'backward-symbol-at-point)


;; (gsk "<mouse-4>" 'backward-symbol-at-point)


;; Would be better...
;; M-SPC toggles symbol at point as a highlight & candidate for M-n M-p

(defun forward-search-region (min max)
  (interactive "r")
  (let ((str (buffer-substring-no-properties min max)))
    (unless (search-forward-regexp (regexp-quote str)) (error "region not found"))
    (push-mark (match-beginning 0))
    (activate-mark)))

(defun backward-search-region (min max)
  (interactive "r")
  (let ((str (buffer-substring-no-properties min max)))
    (unless (save-excursion
              (goto-char min)
              (search-backward-regexp (regexp-quote str))) (error "region not found"))
    (goto-char (match-end 0))
    (push-mark (match-beginning 0))
    (activate-mark)))
