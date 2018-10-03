;;
;; PIKA WIP
;;

(defun insert-current-hhmm ()
  (interactive)
  (when (region-active-p) (delete-region (point) (mark)))
  (insert (format-time-string "%H%M" (current-time))))

(defun insert-current-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))
(global-set-key (kbd "C-c t") #'insert-current-date)

(defun pika-here ()
  (interactive)
  (let ((pika-buffer "pikatock-output")
        (f (buffer-file-name)))
    (switch-to-buffer-other-window pika-buffer)
    (fundamental-mode)
    (erase-buffer)
    (setq-local show-trailing-whitespace nil)
    (shell-command (string-join (list "pikatock -dd" f) " ") t)
    (end-of-buffer)
    (insert "\n\nWeek summary:\n")
    (shell-command (string-join (list "pikatock " f) " ") t)
    (view-mode)
    (end-of-buffer)))

(defvar pikatock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'insert-current-hhmm)
    (define-key map (kbd "C-c t") #'insert-current-date)
    (define-key map (kbd "C-c k") #'pika-here)
    map))

(defun pika-indent-function ()
  ;; FIXME What am I smoking?
  (let ((before-column (- (point) (point-at-bol) (current-indentation))) ;; Current column, relative to indentation
        (c (char-after (+ (current-indentation) (point-at-bol))))) ;; First character on line
    (indent-line-to
     (cond ((and (not (null c)) (char-equal ?- c)) 4)
           ((= (point) (point-at-bol)) 4)
           (0)))
    (set-window-point nil
                      (max
                       (+ (point-at-bol) (current-indentation) before-column) ;; Restore previous position which was after indent
                       (+ (point-at-bol) (current-indentation)))))) ;; Move point to first character on line

(defvar pikatock-highlights '(
                              ("^....-..-..*$" . font-lock-function-name-face)
                              ("^....-...." . font-lock-variable-name-face)
                              (":" . font-lock-comment-face)
                              ))

(define-derived-mode pikatock-mode
  text-mode "Pikatock" "Major mode for time logs."
  (setq-local indent-line-function #'pika-indent-function)
  (setq-local electric-indent-chars '(?- ?\n))
  (setq-local require-final-newline t)
  (setq-local font-lock-defaults '(pikatock-highlights)))

(add-to-list 'auto-mode-alist '("\\.time\\'" . pikatock-mode))
