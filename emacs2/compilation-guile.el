(require 'compile)

(defvar compilation-guile-font-lock-keywords '())
(defvar compilation-guile-scroll-output t)
(defvar compilation-guile-error-regexp-alist
  '(("^;;; \\([^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3)
    ("^\\([^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$" 1 2 3)
    ("^\\test:.*(\\([^:[:space:]]+\\):\\([[:digit:]]+\\))" 1 2 3 0)
    guile-file
    guile-line))
(defvar compilation-guile-command-history nil)

(define-compilation-mode compilation-guile "compilation guile" nil)

(defun compile-guile (cmd)
  (interactive
   (list (read-from-minibuffer "Command: " nil nil nil 'compilation-guile-command-history)))
  (compilation-start cmd 'compilation-guile
                     (lambda (mode) (concat "*guile compilation* " cmd))))

(provide 'compilation-guile)
