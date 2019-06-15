;; Shrink mode lines
(dolist (f '(mode-line mode-line-inactive))
  (set-face-attribute f nil :height 100 :box nil))