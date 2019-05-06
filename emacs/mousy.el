(defun mousy-line-points ()
  (let* ((height (window-body-height))
	 (top (save-excursion
		(set-window-point nil (window-start))
		(line-number-at-pos)))
	 (bottom (+ top height -1))
	 (middle (+ top (/ height 2))))
    (list top middle bottom)))

(defun mousy-scroll-up ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (pcase-let ((`(,top ,middle ,bottom) (mousy-line-points)))
      (cond ((= line top) (scroll-up))
	    ((<= line middle) (recenter 0))
	    ((recenter))))))

(defun mousy-scroll-down ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (pcase-let ((`(,top ,middle ,bottom) (mousy-line-points)))
      (cond ((= line bottom) (scroll-down))
	    ((>= line middle) (recenter -1))
	    ((recenter))))))

(defconst mousy-mode-map (make-sparse-keymap))
;; (define-key mousy-mode-map (kbd "<wheel-up>") 'mousy-scroll-up)
;; (define-key mousy-mode-map (kbd "<wheel-down>") 'mousy-scroll-down)
(define-key mousy-mode-map (kbd "<M-down>") 'mousy-scroll-up)
(define-key mousy-mode-map (kbd "<M-up>") 'mousy-scroll-down)

(define-minor-mode mousy-mode
  "Minor mode for using the scroll wheel to scroll and recentre."
  :lighter " m"
  :keymap mousy-mode-map
  :global t)
