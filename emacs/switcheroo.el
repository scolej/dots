;; TODO
;; - make whole buffer except filter line unmodifiable
;; - clickable
;; - numpad numbers same as f keys


(defconst switcheroo-buffer-name "*switcheroo*")

(defun switcheroo-create-buffer ()
  (with-current-buffer (get-buffer-create switcheroo-buffer-name)
    (erase-buffer)
    (switcheroo-write-buffer)
    (beginning-of-buffer)
    (switcheroo-mode)
    (setq-local truncate-lines t)
    (add-hook 'after-change-functions 'switcheroo-after-change nil t)
    (current-buffer)))

(defun switcheroo ()
  (interactive)
  (switch-to-buffer (or (get-buffer switcheroo-buffer-name)
                        (switcheroo-create-buffer)))
  (beginning-of-buffer)
  (delete-region (point) (point-line-end))
  (switcheroo-rewrite (current-buffer)))

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

(defun switcheroo-select ()
  (interactive)
  (if (equal 1 (line-number-at-pos))
      (switcheroo-select-1)
    (switcheroo-select-current)))

(defun switcheroo-select-current ()
  (switch-to-buffer
   (get-text-property (point) 'field)))

(defun switcheroo-select-nth (n)
  (beginning-of-buffer)
  (goto-line (1+ n))
  (switcheroo-select-current))

(defun switcheroo-select-1 () (interactive) (switcheroo-select-nth 1))
(defun switcheroo-select-2 () (interactive) (switcheroo-select-nth 2))
(defun switcheroo-select-3 () (interactive) (switcheroo-select-nth 3))
(defun switcheroo-select-4 () (interactive) (switcheroo-select-nth 4))
(defun switcheroo-select-5 () (interactive) (switcheroo-select-nth 5))
(defun switcheroo-select-6 () (interactive) (switcheroo-select-nth 6))
(defun switcheroo-select-7 () (interactive) (switcheroo-select-nth 7))
(defun switcheroo-select-8 () (interactive) (switcheroo-select-nth 8))
(defun switcheroo-select-9 () (interactive) (switcheroo-select-nth 9))

(defun contains-all (words str)
  (seq-every-p
   (lambda (s) (string-match-p s str))
   words))

(defun switcheroo-write-buffer (&optional str)
  (insert "\n")
  ;; FIXME Use window height for max lines?
  (let ((max-lines 25)
        (bufs
         (if (null str) (buffer-list)
           (seq-filter
            (lambda (b)
              (let* ((f (buffer-file-name b))
                     (test-string (string-join (list (buffer-name b)
                                                     (if f f "")))))
                (and (not (minibufferp b))
                     (contains-all (split-string str) test-string))))
            (buffer-list))))
        (counter 1))
    (dolist (b (seq-take bufs max-lines))
      (switcheroo-write-line b counter)
      (setq counter (1+ counter)))))

(defun switcheroo-write-line (b i)
  (if (<= counter 9) (insert (format "%2d " counter)) (insert "   "))
  (insert (string-trim (buffer-name b)))
  (let ((f (buffer-file-name b)))
    (when f (insert " - " f)))
  (put-text-property (point-line-start) (point-line-end) 'field b)
  (insert "\n"))

(defvar-local switcheroo-idle-timer nil)

(defun switcheroo-rewrite (buf)
  (with-current-buffer buf
    (when switcheroo-idle-timer (cancel-timer switcheroo-idle-timer))
    (remove-hook 'after-change-functions 'switcheroo-after-change t)
    (let ((filter-string (save-excursion
                           (beginning-of-buffer)
                           (buffer-substring-no-properties (point) (point-line-end)))))
      (save-excursion
        (beginning-of-buffer)
        (end-of-line)
        (delete-region (point) (point-max))
        (switcheroo-write-buffer filter-string)))
    (add-hook 'after-change-functions 'switcheroo-after-change nil t)))

(defun switcheroo-after-change (a b c)
  ;; FIXME only if change is on first line
  (when switcheroo-idle-timer
    (cancel-timer switcheroo-idle-timer))
  (setq switcheroo-idle-timer (run-at-time 0.3 nil 'switcheroo-rewrite (current-buffer))))

(define-derived-mode switcheroo-mode fundamental-mode " sroo")