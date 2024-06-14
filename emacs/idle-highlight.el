;; TODO
;; - Make a global & minor mode.
;; - Handle case properly.
;; - use a unique face to not blow away manual highlights
;; - use overlays?
;; - Easily jump forward & backward between highlighted locations.
;; - set a local to enable idle highlight symbol
;; - why does it break when switching tabs with active region?

(require 'subr-x)

;; require this so we can test for rectangle-mark-mode
(require 'rect)

(defvar idle-highlight-string nil)

(defun idle-highlight-clean ()
  "Remove any active highlight."
  (when idle-highlight-string
    (unhighlight-regexp idle-highlight-string)
    (setq idle-highlight-string nil)))

(add-hook 'post-command-hook 'idle-highlight-post-command)

(defun idle-highlight-post-command ()
  (if (and (use-region-p)
           (not deactivate-mark)
           (not rectangle-mark-mode))
      (progn
        (idle-highlight-clean)
        (let ((str (buffer-substring-no-properties (point) (mark))))
          (unless (string-blank-p str)
            (setq idle-highlight-string (regexp-quote str))
            (highlight-regexp idle-highlight-string 'hi-aquamarine))))
    (idle-highlight-clean)))

;; (defun idle-highlight-post-command ()
;;   (let ((str (or (and (region-active-p) (not deactivate-mark) (not rectangle-mark-mode)
;;                       (buffer-substring-no-properties (point) (mark)))
;;                  (thing-at-point 'symbol t))))
;;     
;;     (if str
;;         (progn
;;           (idle-highlight-clean)
;;           (setq idle-highlight-string (regexp-quote str))
;;           (highlight-regexp idle-highlight-string 'hi-aquamarine))
;;       (idle-highlight-clean))))

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

