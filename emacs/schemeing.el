;;
;; Paredit & Scheme conf
;;

;; todo
;;
;; recover M-up/down
;; more often want to move sexps than do fancy splicing
;;
;; inserting parens in comments is annoying
;;
;; lispy?

(require 'scheme)
(require 'paredit)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; todo
;; kind of broken
;; - shadows M-r
;; - often breaks on pathological output
;; (add-hook 'inferior-scheme-mode-hook 'paredit-mode)

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

(defun dwim-sexp-copy ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-ring-save)
    (let (beg end)
      (save-excursion
        (forward-sexp)
        (setf end (point))
        (backward-sexp)
        (setf beg (point)))
      (kill-ring-save beg end))))

(defun insert-lambda ()
  (interactive)
  (insert "(λ ())")
  (forward-char -2))

(defun delete-blank-line ()
  "If point is on a line containing only whitespace, delete it all."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (when (string-empty-p (string-trim (buffer-substring-no-properties beg end)))
      (delete-region beg (1+ end)))))

;; maybe better would be something smarter which is more
;; like delete-whole-line.

(defun delete-sexp-around-point ()
  (interactive)
  (paredit-backward-up)
  (delete-forward-sexp)
  (delete-blank-line))

(define-keys paredit-mode-map
  "[" 'paredit-open-round
  "]" 'paredit-close-round
  "(" 'paredit-open-square
  ")" 'paredit-close-square
  "{" 'paredit-wrap-round
  "M-w" 'dwim-sexp-copy
  "M-c" 'clone-sexp
  "C-\\" 'insert-lambda
  ;; "\\" nil
  ;; "C-d" nil

  ;; shadow things which break sexps
  ;; "<C-backspace>" 'delete-backward-sexp
  ;; "<C-S-backspace>" 'delete-sexp-around-point
  ;; "<C-backspace>" nil
  ;; "<C-S-backspace>" nil

  ;; my line moving breaks sexps
  ;; "<M-up>" nil
  ;; "<M-down>" nil

  "M-]" 'paredit-forward
  "M-[" 'paredit-backward
  ;; "M-[" 'paredit-backward-up

  ;; "M-k" 'kill-sexp
  ;; "<M-SPC>" 'squish-space
  ;; "<M-up>" 'paredit-backward-up
  "\\" nil)

(defun squish-space ()
  (interactive)
  (just-one-space -1))

;; todo, format sexp on enter?

(mapc
 (lambda (p)
   (put (car p) 'scheme-indent-function (cdr p)))
 '((apply . 1)
   (match . 1)
   (match-let . 1)
   (set-fields . 1)
   (eval-when . 1)
   (with-error-to-file . 1)
   (let/ec . 1)
   (call-with-prompt . 1)
   (start-stack . 1)
   (test-assert . 1)
   (test-case . 1)
   (let-assq . 2)
   (ctx . 1)
   (while . 1)))

(defun scheme-load-this-file ()
  (interactive)
  (save-buffer)
  (scheme-load-file
   (buffer-file-name)))

;; (define-key scheme-mode-map (kbd "<f11>") 'scheme-load-this-file)



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
  (compilation-start cmd 'compilation-guile))

;; (define-key scheme-mode-map (kbd "<f11>") 'scheme-load-this-file)

