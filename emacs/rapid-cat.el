(defun set-cat (col str)
  (save-excursion
    (beginning-of-line)
    (dotimes (i col) (csv-tab-command))
    (insert str))
  (forward-line))

(define-minor-mode rapid-cat-orange
  "rapidly insert transaction categories for orange account history"
  :keymap
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "g") (lambda () (interactive) (set-cat 5 "expenses:groceries")))
    (define-key km (kbd "b") (lambda () (interactive) (set-cat 5 "expenses:baby")))
    (define-key km (kbd "e") (lambda () (interactive) (set-cat 5 "expenses:eating out")))
    (define-key km (kbd "u") (lambda () (interactive) (set-cat 5 "expenses:utilities")))
    (define-key km (kbd "c") (lambda () (interactive) (set-cat 5 "expenses:car")))
    (define-key km (kbd "p") (lambda () (interactive) (set-cat 5 "expenses:car:petrol")))
    (define-key km (kbd "i") (lambda () (interactive) (set-cat 5 "expenses:home improvements")))
    (define-key km (kbd "t") (lambda () (interactive) (set-cat 5 "expenses:entertainment")))
    (define-key km (kbd "h") (lambda () (interactive) (set-cat 5 "expenses:assorted house")))
    (define-key km (kbd "m") (lambda () (interactive) (set-cat 5 "expenses:medical")))
    (define-key km (kbd "f") (lambda () (interactive) (set-cat 5 "expenses:gifts")))
    (define-key km (kbd "u") (lambda () (interactive) (set-cat 5 "expenses:unknown")))
    (define-key km (kbd "s") (lambda () (interactive) (set-cat 5 "skip")))
    (define-key km (kbd "C-g") 'rapid-cat-orange)
    km))

(define-minor-mode rapid-cat-offset
  "rapidly insert transaction categories for offset account history"
  :keymap
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "t") (lambda () (interactive) (set-cat 3 "income:stile")))
    (define-key km (kbd "l") (lambda () (interactive) (set-cat 3 "expenses:living")))
    (define-key km (kbd "a") (lambda () (interactive) (set-cat 3 "expenses:allowances")))
    (define-key km (kbd "r") (lambda () (interactive) (set-cat 3 "liabilities:home loan:repayments")))
    (define-key km (kbd "s") (lambda () (interactive) (set-cat 3 "skip")))
    (define-key km (kbd "C-g") 'rapid-cat-offset)
    km))
