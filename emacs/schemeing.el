;;
;; Paredit & Scheme conf
;;

;; todo
;; - recover M-up/down
;;   more often want to move sexps than do fancy splicing

(require 'paredit)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; todo
;; kind of broken
;; - shadows M-r
;; - often breaks on pathological output
(add-hook 'inferior-scheme-mode-hook 'paredit-mode)

;; font lock appears mostly to be a disaster in the REPL
(defun disable-font-lock () (font-lock-mode -1))
(add-hook 'inferior-scheme-mode-hook 'disable-font-lock)

(defun clone-sexp ()
  (interactive)
  (let* ((b (point))
         (e (save-excursion
              (forward-sexp)
              (point)))
         (s (buffer-substring-no-properties b e)))
    (insert s)
    (newline-and-indent)))

(defun insert-lambda ()
  (interactive
   (insert "λ")))

(defun delete-sexp-around-point ()
  (interactive)
  (paredit-backward-up)
  (delete-forward-sexp))

(define-keys paredit-mode-map
  "[" 'paredit-open-round
  "]" 'paredit-close-round
  "(" 'paredit-open-square
  ")" 'paredit-close-square
  "M-[" 'paredit-wrap-round
  "<mouse-3>" 'kill-sexp
  "M-c" 'clone-sexp
  "C-\\" 'insert-lambda
  "<C-S-backspace>" 'delete-sexp-around-point
  ;; "\\" nil
  ;; "C-d" nil
  "<M-up>" nil
  "<M-down>" nil)

;; todo, format sexp on enter?

(mapc
 (lambda (p)
   (put (car p) 'scheme-indent-function (cdr p)))
 '((match . 1)
   (match-let . 1)
   (set-fields . 1)
   (eval-when . 1)
   (with-error-to-file . 1)
   (let/ec . 1)
   (call-with-prompt . 1)
   (start-stack . 1)
   (test-assert . 1)
   (test-case . 1)
   (let-assq . 2)))

(defun scheme-load-this-file ()
  (interactive)
  (save-buffer)
  (scheme-load-file
   (buffer-file-name)))



(defvar guile-compilation-font-lock-keywords '())
(defvar guile-compilation-scroll-output nil)
(defvar guile-compilation-error-regexp-alist
  '(("^;;; \\([.[:alnum:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$" 1 2 3)
    ("^\\([.[:alnum:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)$" 1 2 3)
    ("^\\test:.*(\\([.[:alnum:]]+\\):\\([[:digit:]]+\\))" 1 2 3 0)
    guile-file
    guile-line))

(define-compilation-mode guile-compilation "guile compilation" nil)

(defun guile-compile (cmd)
  (interactive "s")
  (compilation-start cmd 'guile-compilation))
