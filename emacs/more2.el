(require 'pick nil t)
(global-set-key (kbd "<f1>") 'pick-select-buffer)
(global-set-key (kbd "<f2>") 'pick-filelist)
(pick-define-function-keys)
(pick-define-numpad-keys)

(require 'selected nil t)
(define-key selected-keymap (kbd "<return>") 'kill-ring-save)
(define-key selected-keymap (kbd "r") 'query-replace-maybe-region)
(global-set-key (kbd "<C-return>") 'yank)
(selected-global-mode)

(load "experiments/search.el")

(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "<f12>") 'save-all)

(setq-default
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %b:%l:%c"))


;; todo
;; stop pressing tab so much

(defun really-kill-buffer ()
  (interactive) (kill-buffer nil))

(require 'cl)
(require 'dired-x)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(defun define-keys (keymap &rest keys)
  "Make multiple bindings in a map."
  (cl-loop for (key binding) on keys by #'cddr do
           (define-key keymap (kbd key) binding)))

(defun keymap (&rest bindings)
  "Make a new keymap with bindings. Return that map."
  (let ((map (make-sparse-keymap)))
    (apply 'define-keys map bindings)
    map))

(global-set-key (kbd "<tab>") 'other-window)

(define-keys minibuffer-local-map
  "<escape>" 'top-level)

(let ((keys (keymap
             "j" 'dired-jump
             "k" 'really-kill-buffer
             "e" 'eval-buffer
             "c" 'new-frame
             "q" 'quit
             "0" 'delete-window
             "1" 'delete-other-windows
             "2" 'split-window-below
             "3" 'split-window-right
             "f" 'find-file
             "g" 'google
             "b" 'switch-to-buffer
             "s" 'isearch-forward
             "h" (keymap "f" 'describe-function
                         "v" 'describe-variable
                         "k" 'describe-key)
             ;; "<escape>" 'top-level
             )))
  (global-set-key (kbd "<escape>") keys))

;;
;; Duplicating
;;

(defun duplicate-region (dir)
  (interactive)
  (let* ((p (point))
         (m (mark))
         (text (buffer-substring-no-properties p m))
         (deactivate-mark nil))
    (save-excursion
      (cond
       ((eq dir 'down) (goto-char (max p m)) (insert text))
       ((eq dir 'up) (goto-char (min p m)) (insert-before-markers text))))))

(defun duplicate-line (dir)
  (interactive)
  (let* ((bol (point-at-bol))
         (eol (point-at-eol))
         (pos-on-line (- (point) bol))
         (text (buffer-substring-no-properties bol eol)))
    (cond ((eq dir 'down) (forward-line))
          ((eq dir 'up) (beginning-of-line)))
    (save-excursion (insert text "\n"))
    (forward-char pos-on-line)))

(defun duplicate (dir)
  (interactive)
  (if (region-active-p)
      (duplicate-region dir)
    (duplicate-line dir)))

(defun duplicate-up () (interactive) (duplicate 'up))
(defun duplicate-down () (interactive) (duplicate 'down))

(global-set-key (kbd "<C-M-up>") 'duplicate-up)
(global-set-key (kbd "<C-M-down>") 'duplicate-down)

;;
;; Dragging
;;

(defun drag (dir)
  (interactive)
  (unless (region-active-p)
    (let* ((bol (point-at-bol))
           (eol (point-at-eol))
           (pos-on-line (- (point) bol))
           (text (buffer-substring-no-properties bol eol)))
      (delete-region bol (progn (forward-line) (point)))
      (cond ((eq dir 'down) (end-of-line) (newline))
            ((eq dir 'up) (forward-line -1) (save-excursion (newline))))
      (save-excursion (insert text))
      (forward-char pos-on-line))))

(defun drag-up () (interactive) (drag 'up))
(defun drag-down () (interactive) (drag 'down))

(global-set-key (kbd "<M-down>") 'drag-down)
(global-set-key (kbd "<M-up>") 'drag-up)

;;
;;
;;

(when (boundp 'terminal-prog)
  (defun term-here ()
    (interactive)
    (start-process "term" nil terminal-prog))
  (global-set-key (kbd "C-x t") 'term-here))

;;
;; Query replace using region
;;

(defun query-replace-maybe-region ()
  (interactive)
  (if (region-active-p)
      (let ((str (buffer-substring-no-properties (point) (mark))))
        (deactivate-mark)
        (goto-char (min (point) (mark)))
        (query-replace-regexp
         str
         (read-from-minibuffer (format "Replace %s with: " str) str)))
    (call-interactively 'query-replace-regexp)))

;;
;; Opening lines
;;

(defun new-line (n)
  (let* ((bol (point-at-bol))
         (indent (buffer-substring-no-properties bol (+ bol (current-indentation)))))
    (if (eq n 'up)
        (progn (goto-char bol)
               (save-excursion (insert "\n"))
               (insert indent))
      (progn (goto-char (point-at-eol))
             (insert "\n")
             (insert indent)))))

(defun new-line-above () (interactive) (new-line 'up))
(defun new-line-below () (interactive) (new-line nil))

(global-set-key (kbd "C-S-o") 'new-line-above)
(global-set-key (kbd "C-o") 'new-line-below)

;;
;;
;;

(require 'paredit)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(define-key paredit-mode-map (kbd "[") 'paredit-open-round)
(define-key paredit-mode-map (kbd "]") 'paredit-close-round)
(define-key paredit-mode-map (kbd "(") 'paredit-open-square)
(define-key paredit-mode-map (kbd ")") 'paredit-close-square)
