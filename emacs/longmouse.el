;;
;; Longmouse
;;
;; Functions and bindings for long-pressing right mouse button for copy / cut.
;;

(defvar longmouse-timer nil)

(defun longmouse-down ()
  (interactive)
  (kill-new (buffer-substring-no-properties (point) (mark)))
  (setq deactivate-mark t
        longmouse-timer-1 (run-at-time 0.3 nil
                                       (lambda ()
                                         (message "Cut!")
                                         (delete-region (point) (mark))))
        ;; longmouse-timer-2 (run-at-time 0.9 nil
        ;;                                (lambda ()
        ;;                                  (message "Buried!")
        ;;                                  ;; (pop kill-ring)
        ;;                                  ;; (setq kill-ring-yank-pointer kill-ring)
        ;;                                  (setq kill-ring-yank-pointer (cdr kill-ring))
        ;;                                  ))
        ))

(defun longmouse-up ()
  (interactive)
  (dolist (timer (list longmouse-timer-1
                       ;; longmouse-timer-2
                       ))
    (when timer (cancel-timer timer))
    (setq timer nil)))

(global-set-key [down-mouse-3] 'longmouse-down)
(global-set-key [mouse-3] 'longmouse-up)
(global-set-key [mouse-2] 'yank)
