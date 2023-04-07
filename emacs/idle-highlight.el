;; TODO
;; - Don't match at point, there's no point.
;; - Make a global & minor mode.
;; - Handle case properly.
;; - idle timer setting could avoid work by testing if string has changed.
;; - use a unique face to not blow away manual highlights
;; - use overlays?
;; - Easily jump forward & backward between highlighted locations.

(require 'subr-x)

(defvar idle-highlight-timer nil)
(defvar idle-highlight-string nil)

(defun idle-highlight-clean ()
  "Remove any active highlight."
  (when idle-highlight-string
    (unhighlight-regexp idle-highlight-string)))

(defun idle-highlight-activate ()
  (add-hook 'post-command-hook 'idle-highlight-set-timer))

(defun idle-highlight-deactivate ()
  (remove-hook 'post-command-hook 'idle-highlight-set-timer)
  (idle-highlight-clean))

(defun idle-highlight-set-timer ()
  (when idle-highlight-timer (cancel-timer idle-highlight-timer))
  (setq idle-highlight-timer
        (run-at-time 0.2 nil 'idle-highlight-region)))

(defun idle-highlight-region ()
  (when mark-active
    (idle-highlight-clean)
    (let ((str (buffer-substring-no-properties (point) (mark))))
      (unless (string-blank-p str)
        (setq idle-highlight-string (regexp-quote str))
        (highlight-regexp idle-highlight-string 'hi-blue)))))

(defun idle-highlight-keep ()
  "Deactivate region & keep the current highlight in a new colour."
  (interactive)
  (deactivate-mark)
  (highlight-regexp idle-highlight-string
                    (let ((hi-lock-auto-select-face t))
                      (hi-lock-read-face-name)))
  (setq idle-highlight-string nil))

(add-hook 'activate-mark-hook 'idle-highlight-activate)
(add-hook 'deactivate-mark-hook 'idle-highlight-deactivate)
