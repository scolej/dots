;; todo make minor mode targeting a specific window which does this on cursor
;; move.

(defun find-other-window ()
  (interactive)
  (let* ((thing (thing-at-point 'word t))
         (pat (regexp-quote thing)))
    (with-selected-window (next-window)
      (goto-char (point-min))
      (re-search-forward pat)
      (unhighlight-regexp t)
      (highlight-lines-matching-regexp pat))))

(defun hh-find-in-window (window)
  (interactive)
  (let* ((thing (thing-at-point 'word t))
         (pat (regexp-quote thing)))
    (with-selected-window window
      (goto-char (point-min))
      (re-search-forward pat)
      (unhighlight-regexp t)
      (highlight-lines-matching-regexp pat))))

(defun handy-highlight-thing ()
  (when handy-highlight-other-window
    (hh-find-in-window handy-highlight-other-window)))

(define-minor-mode handy-highlight-mode
  "Minor mode to highlight the thing at point in another window."
  :lighter " hh"
  :global nil
  (if handy-highlight-mode
      (progn
       (setq-local handy-highlight-other-window (next-window))
       (add-hook 'post-command-hook 'handy-highlight-thing))
    (remove-hook 'post-command-hook 'handy-highlight-thing)))
