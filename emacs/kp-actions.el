(defun kp-actions-on () (kp-actions 1))
(defun kp-actions-off () (kp-actions -1))

(define-minor-mode kp-actions
  "Quick actions on the keypad."
  :global t
  :keymap
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<kp-1>") 'pick-select-buffer)
    (define-key km (kbd "<kp-4>") 'goto-line)
    (define-key km (kbd "<kp-5>") 'swiper-selection)
    km)
  (add-hook 'minibuffer-setup-hook 'kp-actions-off)
  (add-hook 'minibuffer-inactive-mode-hook 'kp-actions-on))



