(require 'flycheck)

(setq
 flycheck-display-errors-delay 3
 flycheck-display-errors-function 'flycheck-display-error-messages)

;; https://github.com/flycheck/flycheck/issues/1820
;; borked :[
(setq-default flycheck-disabled-checkers '(haskell-hlint))

(define-key flycheck-mode-map
  (kbd "C-c h") 'flycheck-display-error-at-point)
