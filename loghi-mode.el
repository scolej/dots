;; Major mode for highlighting bits of logs. (Maybe should be a minor
;; mode? Can you even do highlighting for them?)

(defface face-log-error    '((t (:background "red")))    "")
(defface face-log-warning  '((t (:background "orange"))) "")
(defface face-log-info     '((t (:foreground "blue")))   "")

(setq my-hi '(("INFO" . 'face-log-info)
              ("WARNING" . 'face-log-warning)
              ("ERROR\\|SEVERE" . 'face-log-error)))

(define-derived-mode loghi-mode fundamental-mode
  (setq font-lock-defaults '(my-hi))
  (setq mode-name "loghi"))
