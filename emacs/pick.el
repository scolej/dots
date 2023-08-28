;;; -*- lexical-binding: t -*-

;; Ideas
;;
;; what is shown initially (when no filter) is somewhat useless
;; would be better to keep track of common selections, and peg them to a number
;; so you can organically associate a buffer to a number
;;
;; colour code lines by major mode ... but keep it general, this should work easily with anything
;;
;; allow filter string to select by major mode
;;
;; TODO
;; - M-p M-n cycle through history
;; - open buffer in other window, in v/h split
;; - a read-only character after first line to stop contents sneaking onto first line
;; - buffer picking: append recent files
;;      common pattern of looking for a buffer which is not open
;;      but was recently open
;;
;; ??
;; - f2 -> files in project which are open in a buffer
;; - C-f2 -> all files in project
;;
;; pick-rename-buffer - easily spin off a separate long-lived pick buffer;
;; eg: write a filter for all the markdowns under a dir, then split off a
;; new buffer with this search. can then easily switch back to it, hit g to
;; update as necessary.
;;
;; ===> multi lines of filters?
;;
;; use buttons!

(require 'subr-x)
(require 'seq)

(defvar-local pick-options nil
  "The list of candidates we're selecting from in this buffer.")

(defun pick-buffer (name options)
  "Create a picking buffer named NAME and fill it with OPTIONS. An
option is a pair where the first element is the string to display
and the second element is a function to call when the option is
selected."
  ;; (let ((buf (get-buffer name)))
  ;;   (when buf (kill-buffer buf)))
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (remove-hook 'after-change-functions 'pick-after-change t)
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
    (dolist (o (seq-take options 100))
      (pick-write-line i (car o) (cdr o))
      (setq i (1+ i)))))

(defun pick-write-line (i text action)
  (if (<= i 9) (insert (format "%2d " i)) (insert "   "))
  (let ((p0 (point)))
    (insert (string-trim text))
    (make-text-button p0 (point) 'action (lambda (unused) (funcall action)))
    (insert "\n")))

;;
;;
;;

(defcustom pick-idle-delay 0.3
  "Seconds to wait until refreshing the picking buffer.")

(defvar-local pick-idle-timer nil
  "Timer for rewriting the pick buffer after filter input has
  changed.")

(defun pick-after-change (beg end pre)
  "Hook run on buffer change.
Change is from BEG to END with PRE chars previously in this
range."
  ;; Only do anything if the change is on the first line.
  (when (let ((p0 (point-min))
              (p1 (save-excursion (beginning-of-buffer)
                                  (point-at-eol))))
          (or (<= p0 beg p1)
              (<= p0 end p1)))
    (when pick-idle-timer (cancel-timer pick-idle-timer))
    (setq pick-idle-timer
          (run-at-time pick-idle-delay nil 'pick-rewrite (current-buffer)))))

(defun contains-all (words str)
  "Return t if every element of the list WORDS is a substring of STR."
  (seq-every-p
   (lambda (s)
     (string-match-p (regexp-quote s) str))
   words))

(defun contains-none (words str)
  "Return t if no element in the list WORDS is a substring of STR."
  (seq-every-p
   (lambda (s)
     (not (string-match-p (regexp-quote s) str)))
   words))

;; This is way harder than it looks. Consider the query string "regisreadmemd"
;; and 2 cases "data-warehouse-service/migrations/README-FIRST.md" and
;; "operations/docker/registry/README.md". Because "re" hits "waREhouse" and "r"
;; hits "opeRations", the first one is a better match even though if you skipped
;; the first "r" in the second case then it'd likely win. You almost have to,
;; first consider all the "r"s, then extend the search to "re" and see if any of
;; the previous "r" hits can be extended, then take that one and so on. ie: keep
;; extending the search string until there's no match, then use the longest hit
;; as the hit, drop that part of the search string, and continue. just requiring
;; the user to delimit with spaces makes the whole thing easier and more precise
;; and faster.
;;
;; (defun fuzzy-score (query test-string)
;;   (setf test-string (downcase test-string))
;;   (catch 'done
;;     (let ((score 0))
;;       (dolist (c (string-to-list query) score)
;;         (when (> 0 score) (throw 'done 0))
;;         (let ((pos (seq-position test-string c)))
;;           (cond
;;            ((null pos)
;;             (setf score (- score 1)))
;;            ((= pos 0)
;;             (setf score (+ 2 score)
;;                   test-string (substring test-string 1)))
;;            ((> pos 0)
;;             (setf score (+ 1 score)
;;                   test-string (substring test-string (1+ pos))))))))))

;; (defun pick-filter (str options)
;;   (seq-sort-by
;;    (lambda (x) (fuzzy-score str (car x)))
;;    #'>
;;    options))

;; (defun pick-filter (str options)
;;   (mapcar
;;    #'cdr
;;    (seq-sort-by
;;     #'car #'>
;;     (seq-filter
;;      (lambda (x) (> (car x) 0))
;;      (mapcar
;;       (lambda (x) (cons (fuzzy-score str (car x)) x))
;;       options))))

;; todo map, filter then sort

(defun pick-filter (str options)
  (let* ((words (split-string str))
         (groups (seq-group-by (lambda (w) (string-prefix-p "!" w)) words))
         (pwords (alist-get nil groups '()))
         (nwords (mapcar (lambda (s) (substring s 1)) (alist-get t groups '()))))
    (seq-filter
     (lambda (o)
       (and (contains-all pwords (car o))
            (contains-none nwords (car o))))
     options)))

(defun pick-rewrite (buf)
  "Rewrite the pick buffer into BUF."
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
         (pick-filter filter-string pick-options))))
    (add-hook 'after-change-functions 'pick-after-change nil t)))

;;
;;
;;

(defun forward-past-number ()
  (forward-char 3))

(defun pick-select-dwim ()
  (interactive)
  (if (equal 1 (line-number-at-pos))
      (pick-select-1)
    (pick-select-current)))

(defun pick-select-current ()
  "Little wrapper around push-button so we can press enter when
point is at the start of the line before the button."
  (beginning-of-line)
  (forward-past-number)
  (push-button))

(defun pick-select-nth (n)
  (save-excursion
    (goto-char (point-min))
    (goto-line (1+ n))
    (pick-select-current)))

(dolist (i (number-sequence 1 9))
  (fset (intern (concat "pick-select-" (number-to-string i)))
        (lambda () (interactive) (pick-select-nth i))))

;;
;;
;;

(defvar pick-mode-map (make-sparse-keymap))
(define-key pick-mode-map (kbd "C-g") 'quit-window)
(define-key pick-mode-map (kbd "<return>") 'pick-select-dwim)
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

;; (defun pick-select-buffer ()
;;   (interactive)
;;   (let ((bufname "*pick buffer*"))
;;     (if (get-buffer bufname) (switch-to-buffer bufname)
;;       (pick-buffer
;;        bufname
;;        (mapcar
;;         (lambda (b)
;;           (cons (let ((bf (buffer-file-name b))
;;                       (bn (buffer-name b)))
;;                   (if bf (concat bn " " bf) bn))
;;                 (lambda () (switch-to-buffer b))))
;;         (seq-filter
;;          (lambda (b)
;;            (let ((n (buffer-name b)))
;;              (not (or (string-prefix-p " *Minibuf" n)
;;                       (equal bufname n)))))
;;          (buffer-list)))))))

;; todo - select buffer in same mode / with same extension

(defun pick-select-buffer (arg)
  "Select buffers.
With a prefix arg, just jump back to previous pick buffer. This
allows you to easily re-use the previous filter."
  (interactive "P")
  (let ((bufname "*pick buffer*"))
    (if (and arg (get-buffer bufname)) (switch-to-buffer bufname)
      (pick-buffer
       bufname
       (mapcar
        (lambda (b)
          (cons (let ((bf (buffer-file-name b))
                      (bn (buffer-name b)))
                  (if bf (concat bn " " bf) bn))
                (lambda () (switch-to-buffer b))))
        (seq-filter
         (lambda (b)
           (let ((n (buffer-name b)))
             (not (or (string-prefix-p " *Minibuf" n)
                      (equal bufname n)))))
         (buffer-list)))))))

(defun pick-git (prefix)
  (interactive "P")
  (let ((bufname "*pick git files*")
        (default-directory (or (locate-dominating-file default-directory ".git") (error "not in a git repo"))))
    (if (and prefix (get-buffer bufname))
        (switch-to-buffer bufname)
      (progn
        (when (get-buffer bufname) (kill-buffer bufname))
        ;; (call-process "git" nil (get-buffer-create bufname) nil "ls-tree" "-r" "--name-only" "HEAD")
        (call-process-shell-command "git ls-tree -r --name-only HEAD | grep -v -e web-client/vendorPackages -e web-client/vendorModules -e wiris -e base-images/gitlab-container-registry" nil bufname )
        (pick-buffer
         bufname
         (mapcar
          (lambda (f) (cons f (lambda () (find-file (concat default-directory f)))))
          (split-string (with-current-buffer bufname (buffer-string)) "\n" t)))))))

;; (add-to-list 'tab-line-exclude-modes 'pick-mode)

(provide 'pick)
