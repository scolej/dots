;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(global-set-key (kbd "M-`") 'jump-to-mark)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
