;;
;; Compilation
;;

(defun compilation-buffer-p (b)
  "Is the given buffer in a mode derived from compilation mode?"
  (with-current-buffer b
    (or (eq major-mode 'comint-mode)
        (derived-mode-p 'compilation-mode))))

(defun find-recent-compilation-buffer ()
  "Finds the most recent buffer which is a compilation mode. (Or
the current buffer, if it's a compilation mode!)"
  (let ((b (current-buffer)))
    (if (compilation-buffer-p b) b
      (seq-find 'compilation-buffer-p
                (buffer-list (selected-frame))))))

(defun recompile-recent-compilation (arg)
  (interactive "P")
  (save-some-buffers t)
  (let ((b (find-recent-compilation-buffer)))
    (if b (with-current-buffer b
            (if arg (call-interactively 'recompile)
              (recompile)))
      (call-interactively 'compile))))

(setq-default compilation-always-kill t)

;; FIXME does nothing?
(add-hook 'compilation-mode-hook
          'ansi-color-for-comint-mode-on)

(setq compilation-always-kill t
      compilation-mode-font-lock-keywords nil)

(gsk "<f11>" 'recompile-recent-compilation)
(gsk "<kp-add>" 'next-error)
(gsk "<kp-subtract>" 'previous-error)
