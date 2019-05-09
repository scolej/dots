(defmacro with-mousy-points (expr)
  `(let* ((height (window-body-height))
	  (top-line (line-number-at-pos (window-start)))
	  (bottom-line (+ top-line height -1))
	  (middle-line (+ top-line (/ height 2)))
          (current-line (line-number-at-pos (point))))
     ,expr))

(defmacro debug-mousy-points (expr)
  `(progn
     (with-mousy-points
      (progn
        (message "Before: %i %i %i %i" top-line middle-line bottom-line current-line)
        ,expr))
     (with-mousy-points
      (message "After: %i %i %i %i" top-line middle-line bottom-line current-line))))

(defun mousy-scroll-up ()
  (interactive)
  (debug-mousy-points
   (cond ((= current-line top-line) (scroll-up))
	 ((<= current-line middle-line) (recenter 0))
	 ((recenter)))))

(defun mousy-scroll-down ()
  (interactive)
  (debug-mousy-points
   (cond ((= current-line bottom-line) (scroll-down))
	 ((>= current-line middle-line) (recenter -1))
	 ((recenter)))))

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
