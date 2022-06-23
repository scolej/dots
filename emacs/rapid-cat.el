;; todo would hydra do this more easily and with a help popup?

(defun rapid-cat-next ()
  (interactive)
  (deactivate-mark)
  (unless (re-search-forward "\\(expenses\\|income\\):unknown" nil t nil)
    (rapid-cat-orange -1)
    (error "no replacements remain"))
  (goto-char (match-beginning 0))
  (setq deactivate-mark nil)
  (push-mark (point) nil t)
  (goto-char (match-end 0)))

(defun insert-and-continue (txt)
  (interactive)
  (delete-region (region-beginning) (region-end))
  (insert txt)
  (rapid-cat-next))

(define-minor-mode rapid-cat-orange
  "rapidly insert transaction categories for orange account history"
  :keymap
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "a") (lambda () (interactive) (insert-and-continue "adjustments")))
    (define-key km (kbd "g") (lambda () (interactive) (insert-and-continue "expenses:groceries")))
    (define-key km (kbd "b") (lambda () (interactive) (insert-and-continue "expenses:baby")))
    (define-key km (kbd "e") (lambda () (interactive) (insert-and-continue "expenses:eating out")))
    (define-key km (kbd "u") (lambda () (interactive) (insert-and-continue "expenses:utilities")))
    (define-key km (kbd "c") (lambda () (interactive) (insert-and-continue "expenses:car")))
    (define-key km (kbd "p") (lambda () (interactive) (insert-and-continue "expenses:car:petrol")))
    (define-key km (kbd "k") (lambda () (interactive) (insert-and-continue "expenses:car:parking")))
    (define-key km (kbd "i") (lambda () (interactive) (insert-and-continue "expenses:home improvements")))
    (define-key km (kbd "t") (lambda () (interactive) (insert-and-continue "expenses:entertainment")))
    (define-key km (kbd "h") (lambda () (interactive) (insert-and-continue "expenses:assorted house")))
    (define-key km (kbd "m") (lambda () (interactive) (insert-and-continue "expenses:medical")))
    (define-key km (kbd "f") (lambda () (interactive) (insert-and-continue "expenses:gifts")))
    (define-key km (kbd "o") (lambda () (interactive) (insert-and-continue "assets:offset")))
    (define-key km (kbd "n") 'rapid-cat-next)
    (define-key km (kbd "C-g") (lambda () (interactive ) (rapid-cat-orange -1)))
    km)
  (when rapid-cat-orange
    (rapid-cat-next)))
