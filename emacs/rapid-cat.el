;; -*- lexical-binding: t -*-

;; todo would hydra do this more easily and with a help popup?
;;
;; trouble is the nesting story, you can nest them, but it's not that nice
;; https://github.com/abo-abo/hydra/wiki/Nesting-Hydras

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

(defun make-insertion (txt)
  (lambda () (interactive) (insert-and-continue txt)))

(defun rapid-cat-quit ()
  (interactive)
  (rapid-cat-orange -1)
  (deactivate-mark)
  (message "finished rapid cat"))

(defun delete-paragraph ()
  (interactive)
  (deactivate-mark)
  (let ((beg nil)
        (end nil))
    (backward-paragraph)
    (forward-line)
    (setq beg (point))
    (forward-paragraph)
    (setq end (point))
    (delete-region beg end)
    (delete-blank-lines))
  (rapid-cat-next))

(defun keymap (&rest bindings)
  "Make a new keymap with bindings. Return that map."
  (let ((map (make-sparse-keymap)))
    (apply 'define-keys map bindings)
    map))

(define-minor-mode rapid-cat-orange
  "rapidly insert transaction categories for orange account history"
  :keymap
  (keymap
   "<tab>" 'rapid-cat-next
   "<backtab>" 'jump-to-mark
   "<escape>" 'rapid-cat-quit
   "C-g" 'rapid-cat-quit
   "d" 'delete-paragraph
   "a" (keymap "o" (make-insertion "assets:offset")
               "r" (make-insertion "assets:orange"))
   "l" (keymap "e" (make-insertion "expenses:allowance:emma")
               "s" (make-insertion "expenses:allowance:shannon"))
   "b" (make-insertion "expenses:baby")
   "c" (keymap "p" (make-insertion "expenses:car:petrol")
               "r" (make-insertion "expenses:car:rego")
               "i" (make-insertion "expenses:car:insurance")
               "t" (make-insertion "expenses:car:tolls"))
   "h" (keymap "h" (make-insertion "expenses:assorted house")
               "r" (make-insertion "expenses:assorted house:rates")
               "c" (make-insertion "expenses:assorted house:owners corp")
               "i" (make-insertion "expenses:assorted house:insurance"))
   "e" (make-insertion "expenses:eating out")
   "f" (make-insertion "expenses:gifts")
   "g" (make-insertion "expenses:groceries")
   "i" (make-insertion "expenses:home improvements")
   "k" (make-insertion "expenses:car:parking")
   "m" (make-insertion "expenses:medical")
   "r" (make-insertion "expenses:public transport")
   "t" (make-insertion "expenses:entertainment")
   "u" (keymap "w" (make-insertion "expenses:utilities:water")
               "e" (make-insertion "expenses:utilities:elec")
               "g" (make-insertion "expenses:utilities:gas")
               "i" (make-insertion "expenses:utilities:internet")
               "t" (make-insertion "expenses:utilities:tip")))
  (when rapid-cat-orange
    (rapid-cat-next)))

(define-minor-mode rapid-cat-idle
  "a single binding to easily start rapid catting"
  :keymap (keymap "C-c C-c" (lambda () (interactive) (rapid-cat-orange 1))))
