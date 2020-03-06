;;; switcheroo.el --- Buffer switching with filtering and single key selection.  -*- lexical-binding: t -*-

;;; Commentary:

;; Buffer switcher which attempts to combine the styles of Ivy and
;; Avy: filtering and single key selection from many candidates.
;; Invoke `switcheroo' to jump to the switcheroo buffer. Here, text
;; can be entered on the first line to filter for matching buffer
;; candidates, which are listed below. Candidates can be immediately
;; selected by number. Alternatively, point can be moved to the
;; appropriate line and then selected with `switcheroo-select'.

;; The default bindings use the function keys and number-keypad to
;; select a candidate buffer by number.

;; Candidates are listed by recency of display. Matching is performed
;; by taking the first line, splitting it on spaces into words and
;; then finding buffers whose name or visited file (if it exists)
;; contain subtring matches for every word.

;; TODO & Ideas
;; - make whole buffer except filter line unmodifiable
;; - clickable lines
;; - exclude visible buffers? buffers on same window?
;; - fuzzy filter, don't require exact matches

;;; Code:

(defconst switcheroo-buffer-name "*switcheroo*")

(defcustom switcheroo-max-lines 20
  "Maximum number of lines to show in the switcheroo buffer.")

(defun switcheroo-create-buffer ()
  "Set up the switcheroo buffer, creating it if necessary."
  (with-current-buffer (get-buffer-create switcheroo-buffer-name)
    (erase-buffer)
    (switcheroo-write-buffer)
    (goto-char (point-min))
    (switcheroo-mode)
    (setq-local truncate-lines t)
    (add-hook 'after-change-functions 'switcheroo-after-change nil t)
    (current-buffer)))

(defun switcheroo ()
  "Opens the switcheroo buffer in the current buffer to select a new buffer."
  (interactive)
  (switch-to-buffer (or (get-buffer switcheroo-buffer-name)
                        (switcheroo-create-buffer)))
  (goto-char (point-min))
  (delete-region (point) (point-line-end))
  (switcheroo-rewrite (current-buffer)))

(defun switcheroo-select ()
  "Switch to a buffer depending on point's position.
Select the first candidate if point is on the first line,
otherwise the candidate on the current line."
  (interactive)
  (if (equal 1 (line-number-at-pos))
      (switcheroo-select-1)
    (switcheroo-select-current)))

(defun switcheroo-select-current ()
  "Examine text property 'field under point and switch to that buffer."
  (switch-to-buffer
   (get-text-property (point) 'field)))

(defun switcheroo-select-nth (n)
  "Switch to the buffer on the Nth line."
  (goto-char (point-min))
  (goto-line (1+ n))
  (switcheroo-select-current))

(dolist (i (number-sequence 1 9))
  (fset (intern (concat "switcheroo-select-" (number-to-string i)))
        (lambda () (interactive) (switcheroo-select-nth i))))

(defun contains-all (words str)
  "Return t if every element of the list WORDS is a substring of STR."
  (seq-every-p
   (lambda (s) (string-match-p s str))
   words))

(defun switcheroo-write-buffer (&optional str)
  "Write the buffer contents into the current buffer.
STR is a string used for filtering buffers. Point is assumed to
be at the start of the buffer."
  (insert "\n")
  (let* (;; Drop the first 2 elements because they are:
         ;;   1. switcheroo buffer itsself
         ;;   2. the buffer where we started
         (candidate-buffers (cddr (buffer-list)))
         (bufs
          (if (null str) candidate-buffers
            (seq-filter
             (lambda (b)
               (let* ((f (buffer-file-name b))
                      (test-string (string-join (list (buffer-name b)
                                                      (if f f "")))))
                 (and (not (minibufferp b))
                      (contains-all (split-string str) test-string))))
             candidate-buffers)))
         (counter 1))
    (dolist (b (seq-take
                bufs
                switcheroo-max-lines))
      (switcheroo-write-line b counter)
      (setq counter (1+ counter)))))

(defun switcheroo-write-line (b i)
  "Write a line for buffer B. Line is the Ith candidate."
  (if (<= i 9) (insert (format "%2d " i)) (insert "   "))
  (insert (string-trim (buffer-name b)))
  (let ((f (buffer-file-name b)))
    (when f (insert " - " f)))
  (put-text-property (point-line-start) (point-line-end) 'field b)
  (insert "\n"))

(defvar-local switcheroo-idle-timer nil
  "Timer for rewriting the switcheroo buffer after filter input
  has changed.")

(defun switcheroo-rewrite (buf)
  "Rewrite the switcheroo buffer into BUF.
Adding and remov hooks/timers as necessary."
  (with-current-buffer buf
    (when switcheroo-idle-timer (cancel-timer switcheroo-idle-timer))
    (remove-hook 'after-change-functions 'switcheroo-after-change t)
    (let ((filter-string (save-excursion
                           (beginning-of-buffer)
                           (buffer-substring-no-properties (point) (point-line-end)))))
      (save-excursion
        (goto-char (point-min))
        (end-of-line)
        (delete-region (point) (point-max))
        (switcheroo-write-buffer filter-string)))
    (add-hook 'after-change-functions 'switcheroo-after-change nil t)))

(defun switcheroo-after-change (beg end pre)
  "Hook run on buffer change.
Change is from BEG to END with PRE chars previously in this
range."
  ;; FIXME only if change is on first line
  (when switcheroo-idle-timer
    (cancel-timer switcheroo-idle-timer))
  (setq switcheroo-idle-timer
        (run-at-time 0.3 nil
                     'switcheroo-rewrite
                     (current-buffer))))

(defvar switcheroo-mode-map (make-sparse-keymap))
(define-key switcheroo-mode-map (kbd "C-g") 'quit-window)
(define-key switcheroo-mode-map (kbd "<return>") 'switcheroo-select)
(define-key switcheroo-mode-map (kbd "<f1>") 'switcheroo-select-1)
(define-key switcheroo-mode-map (kbd "<f2>") 'switcheroo-select-2)
(define-key switcheroo-mode-map (kbd "<f3>") 'switcheroo-select-3)
(define-key switcheroo-mode-map (kbd "<f4>") 'switcheroo-select-4)
(define-key switcheroo-mode-map (kbd "<f5>") 'switcheroo-select-5)
(define-key switcheroo-mode-map (kbd "<f6>") 'switcheroo-select-6)
(define-key switcheroo-mode-map (kbd "<f7>") 'switcheroo-select-7)
(define-key switcheroo-mode-map (kbd "<f8>") 'switcheroo-select-8)
(define-key switcheroo-mode-map (kbd "<f9>") 'switcheroo-select-9)
(define-key switcheroo-mode-map (kbd "<kp-1>") 'switcheroo-select-1)
(define-key switcheroo-mode-map (kbd "<kp-2>") 'switcheroo-select-2)
(define-key switcheroo-mode-map (kbd "<kp-3>") 'switcheroo-select-3)
(define-key switcheroo-mode-map (kbd "<kp-4>") 'switcheroo-select-4)
(define-key switcheroo-mode-map (kbd "<kp-5>") 'switcheroo-select-5)
(define-key switcheroo-mode-map (kbd "<kp-6>") 'switcheroo-select-6)
(define-key switcheroo-mode-map (kbd "<kp-7>") 'switcheroo-select-7)
(define-key switcheroo-mode-map (kbd "<kp-8>") 'switcheroo-select-8)
(define-key switcheroo-mode-map (kbd "<kp-9>") 'switcheroo-select-9)

(define-derived-mode switcheroo-mode fundamental-mode " sroo")

(provide 'switcheroo)

;;; switcheroo.el ends here
