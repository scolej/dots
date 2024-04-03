;;; -*- lexical-binding: t -*-

;; todo for files, only show full path, but abbreviate home dir etc

;; todo should be able to append a line number and go straight to that line

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
;;
;; todo - when sorting filter hits, sort those that match chunks in order first

(require 'subr-x)
(require 'seq)

(defun pick-buffer (name options)
  "Create a picking buffer named NAME and fill it with OPTIONS. An
option is a pair where the first element is the string to display
and the second element is a function to call when the option is
selected."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (remove-hook 'after-change-functions 'pick-after-change t)
      (erase-buffer)
      (insert "\n")
      (pick-write-buffer options)
      (goto-char (point-min))
      (pick-mode)
      (setq pick-all-candidates options)
      (pick-reset)
      (add-hook 'after-change-functions
                'pick-after-change nil t))
    (switch-to-buffer buf)))

(defun pick-write-buffer (options)
  (pick-write-candidates (seq-take options pick-max-candidates-to-display)))

(defun pick-write-candidates (options)
  (dolist (o options)
    (pick-write-line pick-current-number (car o) (cdr o))
    (setq pick-current-number (1+ pick-current-number))))

(defun pick-write-line (i text action)
  (if (<= i 9) (insert (format "%2d " i)) (insert "   "))
  (let ((p0 (point)))
    (insert (string-trim text))
    (make-text-button p0 (point) 'action (lambda (unused) (funcall action)))
    (insert "\n")))


(defcustom pick-max-candidates-to-display 40
  "Maximum number of candidates to print in the selection buffer")

(defvar-local pick-timer nil)
(defvar-local pick-all-candidates nil)
(defvar-local pick-remaining-chunks nil)
(defvar-local pick-current-number 1)

(defun pick-reset ()
  (setq pick-remaining-chunks (seq-split pick-all-candidates 50)
        pick-current-number 1))

(defun pick-do-chunk ()
  (when (and pick-remaining-chunks
             (< pick-current-number pick-max-candidates-to-display))
    (let* ((filter (pick-read-filter-string))
           (matching (pick-filter filter (car pick-remaining-chunks))))
      (setq pick-remaining-chunks (cdr pick-remaining-chunks))
      ;; (message "remaing count %d" (length pick-remaining-chunks))
      (remove-hook 'after-change-functions 'pick-after-change t)
      (save-excursion
        (goto-char (point-max))
        (if (eq (line-number-at-pos) 1) (insert "\n"))
        (pick-write-candidates matching))
      (add-hook 'after-change-functions 'pick-after-change nil t)
      (setq pick-timer (run-at-time nil nil 'pick-do-chunk)))))

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
    (when pick-timer (cancel-timer pick-timer))
    (pick-erase-candidates)
    (pick-reset)
    (setq pick-timer (run-at-time 0.2 nil 'pick-do-chunk))))

(defun pick-filter (str options)
  (let* ((words (split-string str))
         (groups (seq-group-by (lambda (w) (string-prefix-p "!" w)) words))
         (pwords (mapcar #'downcase (alist-get nil groups '())))
         (nwords (mapcar (lambda (s) (downcase (substring s 1))) (alist-get t groups '()))))
    (seq-filter
     (lambda (o)
       (let ((str (downcase (car o))))
         (and (contains-all pwords str)
              (contains-none nwords str))))
     options)))

(defun pick-erase-candidates ()
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (delete-region (point) (point-max))))

(defun pick-read-filter-string ()
  (save-excursion
    (beginning-of-buffer)
    (buffer-substring-no-properties (point) (point-at-eol))))

(defun pick-rewrite (buf)
  "Rewrite the pick buffer into BUF."
  (with-current-buffer buf
    (when pick-timer (cancel-timer pick-timer))
    (remove-hook 'after-change-functions 'pick-after-change t)
    (let ((filter-string (pick-read-filter-string)))
      (pick-erase-candidates)
      ;; todo include the unordered matches as well
      (save-excursion
        (insert "\n")
        (pick-write-buffer (pick-filter filter-string pick-options))))
    (add-hook 'after-change-functions 'pick-after-change nil t)))

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
        ;; (call-process-shell-command
        ;;  "find . -type d"
        ;;  nil bufname)
        (call-process-shell-command
         "git ls-tree -r --name-only HEAD | grep -v -e web-client/vendorPackages -e web-client/vendorModules -e wiris -e base-images/gitlab-container-registry"
         nil bufname)
        (pick-buffer
         bufname
         (mapcar
          (lambda (f) (cons f (lambda () (find-file (concat default-directory f)))))
          (split-string (with-current-buffer bufname (buffer-string)) "\n" t)))))))

;; (add-to-list 'tab-line-exclude-modes 'pick-mode)

;;
;;
;;

(defun make-pairs (a b)
  (let ((as (if (listp a) a (list a)))
        (bs (if (listp b) b (list b))))
    (let (vals '())
      (dolist (a as)
        (dolist (b bs)
          (setq vals (cons (cons a b) vals))))
      (reverse vals))))

(ert-deftest test-pairs-1 ()
  (should (equal '((1 . 2))
              (make-pairs 1 2))))
(ert-deftest test-pairs-2 ()
  (should (equal '((1 . 2) (1 . 3))
              (make-pairs 1 '(2 3)))))
(ert-deftest test-pairs-3 ()
  (should (equal '((1 . 3) (1 . 4) (2 . 3) (2 . 4))
              (make-pairs '(1 2) '(3 4)))))
(ert-deftest test-pairs-4 ()
  (should (equal '((1 . 3) (2 . 3))
              (make-pairs '(1 2) 3))))

(defun ordered? (items)
  "Are items ordered? Each element of ITEMS may be either a value or
a list of values. We return T if there is an any ordering of
ITEMS which is ascending. When we encounter a element which is a
list, we test each element of the list as a candidate for
satisfying the ordering."
  (let ((head (car items))
        (tail (cdr items)))
    (if (null tail) t
        (let* ((next (car tail))
               (pairs (make-pairs head next)))
          (if (seq-find
               (lambda (pair) (< (car pair) (cdr pair)))
               pairs)
              (ordered? tail)
            nil)))))

(ert-deftest test1 () (should (ordered? '(1 2 3))))
(ert-deftest test2 () (should (ordered? '((1 4) 2 3))))
(ert-deftest test3 () (should (ordered? '(1 (2 4) 3))))
(ert-deftest test3 () (should (ordered? '(1 2 (1 3)))))
(ert-deftest test4 () (should (not (ordered? '(1 2 (1 1))))))
(ert-deftest test5 () (should (not (ordered? '(5 (2 7) (1 1))))))

(defun string-substring-indices (needle haystack)
  (let ((continue t)
        (result '())
        (i 0))
    (while continue
      (setq i (string-search needle haystack i))
      (if i
          (progn
            (setq result (cons i result)
                  i (1+ i)))
        (setq continue nil)))
    (reverse result)))

(ert-deftest string-substring-test-1 ()
  (should (equal '(1 5) (string-substring-indices "fo" "ofobafo"))))

(defun contains-all (words str)
  "Return t if every element of the list WORDS is a substring of STR."
  (let ((hits (mapcar
               (lambda (word) (string-substring-indices word str))
               words)))
    (and (not (seq-contains-p hits nil))
         (or (= (length hits) 1)
             (ordered? hits)))))

(defun contains-none (words str)
  "Return t if no element in the list WORDS is a substring of STR."
  (seq-every-p
   (lambda (s)
     (not (string-match-p (regexp-quote (downcase s)) (downcase str))))
   words))

;;
;;
;;

(provide 'pick2)
