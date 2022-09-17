(require 'flycheck)

(setq
 flycheck-idle-change-delay 2
 flycheck-display-errors-delay 3
 flycheck-display-errors-function 'flycheck-display-error-messages
 flycheck-check-syntax-automatically '(idle-change))

;; https://github.com/flycheck/flycheck/issues/1820
;; borked :[
(setq-default flycheck-disabled-checkers '(haskell-hlint))

(define-key flycheck-mode-map
  (kbd "C-c h") 'flycheck-display-error-at-point)

