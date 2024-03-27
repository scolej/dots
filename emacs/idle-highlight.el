;; TODO
;; - Don't match at point, there's no point.
;; - Make a global & minor mode.
;; - Handle case properly.
;; - idle timer setting could avoid work by testing if string has changed.
;; - use a unique face to not blow away manual highlights
;; - use overlays?
;; - Easily jump forward & backward between highlighted locations.
;; - set a local to enable idle highlight symbol

(require 'subr-x)

(defvar idle-highlight-string nil)

(defun idle-highlight-clean ()
  "Remove any active highlight."
  (when idle-highlight-string
    (unhighlight-regexp idle-highlight-string)
    (setq idle-highlight-string nil)))

(add-hook 'post-command-hook 'idle-highlight-post-command)

(defun idle-highlight-post-command ()
  (if (region-active-p)
      (progn
        (idle-highlight-clean)
        (let ((str (buffer-substring-no-properties (point) (mark))))
          (unless (string-blank-p str)
            (setq idle-highlight-string (regexp-quote str))
            (highlight-regexp idle-highlight-string 'isearch))))
    (idle-highlight-clean)))

;; todo store the kept strings separately and make them persistent when they're reselected
;; this should be "toggle keep"
(defun idle-highlight-keep ()
  "Deactivate region & keep the current highlight in a new colour."
  (interactive)
  (deactivate-mark)
  (highlight-regexp idle-highlight-string
                    (let ((hi-lock-auto-select-face t))
                      (hi-lock-read-face-name)))
  (setq idle-highlight-string nil))

