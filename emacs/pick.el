;;; -*- lexical-binding: t -*-

;; Ideas
;; what is shown initially (when no filter) is somewhat useless
;; would be better to keep track of common selections, and peg them to a number
;; so you can organically associate a buffer to a number

(require 'subr-x)
(require 'seq)

(defvar-local pick-options nil)

(defun pick-buffer (name options)
  "Create a picking buffer named NAME and fill it with OPTIONS."
  (let ((buf (get-buffer name)))
    (when buf (kill-buffer buf)))
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "\n")
      (pick-write-buffer options)
      (goto-char (point-min))
      (pick-mode)
      (setq pick-options options)
      (add-hook 'after-change-functions
                'pick-after-change nil t))
    (switch-to-buffer buf)))

(defun pick-write-buffer (options)
  (let ((i 1))
    (dolist (o (seq-take options 15))
      (pick-write-line i (car o) (cdr o))
      (setq i (1+ i)))))

(defun pick-write-line (i text action)
  (if (<= i 9) (insert (format "%2d " i)) (insert "   "))
  (insert (string-trim text))
  (put-text-property (point-at-bol) (point-at-eol) 'field action)
  (insert "\n"))

;;
;;
;;

(defvar-local pick-idle-timer nil
  "Timer for rewriting the pick buffer after filter input has
  changed.")

(defun pick-after-change (beg end pre)
  "Hook run on buffer change.
Change is from BEG to END with PRE chars previously in this
range."
  ;; FIXME only if change is on first line
  (when pick-idle-timer
    (cancel-timer pick-idle-timer))
  (setq pick-idle-timer
        (run-at-time 0.3 nil 'pick-rewrite (current-buffer))))

(defun contains-all (words str)
  "Return t if every element of the list WORDS is a substring of STR."
  (seq-every-p
   (lambda (s) (string-match-p s str))
   words))

(defun pick-filter (str options)
  (seq-filter
   (lambda (o) (contains-all (split-string str) (car o)))
   options))

(defun pick-rewrite (buf)
  "Rewrite the pick buffer into BUF.
Adding and remov hooks/timers as necessary."
  (with-current-buffer buf
    (when pick-idle-timer (cancel-timer pick-idle-timer))
    (remove-hook 'after-change-functions 'pick-after-change t)
    (let ((filter-string
           (save-excursion
             (beginning-of-buffer)
             (buffer-substring-no-properties (point) (point-at-eol)))))
      (save-excursion
        (goto-char (point-min))
        (end-of-line)
        (delete-region (point) (point-max))
        (insert "\n")
        (pick-write-buffer
         (funcall 'pick-filter filter-string pick-options))))
    (add-hook 'after-change-functions 'pick-after-change nil t)))

;;
;;
;;

(defun pick-select ()
  (interactive)
  (if (equal 1 (line-number-at-pos))
      (pick-select-1)
    (pick-select-current)))

(defun pick-select-current ()
  "Examine text property 'field under point and invoke that function."
  (let ((f (get-text-property (point) 'field)))
    (funcall f)))

(defun pick-select-nth (n)
  (let ((f (save-excursion
             (goto-char (point-min))
             (goto-line (1+ n))
             (get-text-property (point) 'field))))
    (funcall f)))

(dolist (i (number-sequence 1 9))
  (fset (intern (concat "pick-select-" (number-to-string i)))
        (lambda () (interactive) (pick-select-nth i))))

;;
;;
;;

(defvar pick-mode-map (make-sparse-keymap))
(define-key pick-mode-map (kbd "C-g") 'quit-window)
(define-key pick-mode-map (kbd "<return>") 'pick-select)
(define-derived-mode pick-mode fundamental-mode " pick")

(defun pick-define-function-keys ()
  "Add bindings to pick candidates by number using the function
keys: <f1>, <f2>, <f3>..."
  (dolist (i (mapcar 'number-to-string (number-sequence 1 9)))
    (define-key pick-mode-map
      (kbd (concat "<f" i ">"))
      (intern (concat "pick-select-" i)))))

(defun pick-define-numpad-keys ()
  "Add bindings to pick candidates by number using the numpad
keys: <kp-1>, <kp-2>..."
  (dolist (i (mapcar 'number-to-string (number-sequence 1 9)))
    (define-key pick-mode-map
      (kbd (concat "<kp-" i ">"))
      (intern (concat "pick-select-" i)))))

;;
;;
;;

(defun pick-select-buffer ()
  (interactive)
  (let ((bufname "*pick buffer*"))
    (pick-buffer
     bufname
     (mapcar (lambda (b)
               (cons (let ((bf (buffer-file-name b))
                           (bn (buffer-name b)))
                       (if bf (concat bn " " bf) bn))
                     (lambda () (switch-to-buffer b))))
             (seq-filter
              (lambda (b)
                (let ((n (buffer-name b)))
                  (not (or (string-prefix-p " *Minibuf" n)
                           (equal bufname n)))))
              (buffer-list))))))

(defun pick-filelist ()
  (interactive)
  (pick-buffer
   "*filelist*"
   (let* ((name "filelist")
          (dir (file-name-as-directory
                (locate-dominating-file default-directory name)))
          (filelist (concat dir name))
          items '())
     (with-temp-buffer
       (insert-file-contents filelist)
       (while (< (point) (point-max))
         (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                (f (concat dir line))
                (fun (lambda () (find-file f))))
           (setq items (cons (cons line fun) items))
           (forward-line 1))))
     items)))


(defun pick-list-git-files ()
  (interactive)
  (async-shell-command "git ls-tree -r HEAD | awk '{ print $4 }' > filelist"))

(defun pick-list-gradle-files ()
  (interactive)
  (async-shell-command "git ls-tree -r HEAD | grep gradle | awk '{ print $4 }' > gradles"))

(defun pick-gradles ()
  (interactive)
  (pick-buffer
   "*gradles*"
   (let* ((name "gradles")
          (dir (file-name-as-directory
                (locate-dominating-file default-directory name)))
          (filelist (concat dir name))
          items '())
     (with-temp-buffer
       (insert-file-contents filelist)
       (while (< (point) (point-max))
         (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                (f (concat dir line))
                (fun (lambda () (find-file f))))
           (setq items (cons (cons line fun) items))
           (forward-line 1))))
     items)))

(provide 'pick)
