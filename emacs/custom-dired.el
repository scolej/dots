(require 'dired-x)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(defun dired-find-here (pattern)
  "Find files in this directory using a wildcard pattern."
  (interactive
   (list
    (read-from-minibuffer
     "Find: " '("**" . 2))) )
  (find-name-dired default-directory pattern))

;; (defun dired-git (pattern)
;;   "Find files in this directory using a wildcard pattern."
;;   (interactive
;;    (list
;;     (read-from-minibuffer
;;      "Find: " '("**" . 2))) )
;;   (find-name-dired default-directory pattern))

(defun dired-bro ()
  "Opens the file under point in a browser."
  (interactive)
  (browse-url (dired-filename-at-point)))

;;
;; Easily launch external programs from dired
;;

(defvar dired-launch-programs nil
  "Alist of extensions and program to use.

Each entry can be

- a string: the name of a program which will accept a single
  argument, the file to operate on.

- a procedure of one argument which will return a list
  representing arguments to pass to start-process. ie: the first
  element is the program name, the remaining elements are the
  program's arguments.")

(defun dired-launch ()
  "Launch the file at point in an external program as per entries
in dired-launch-programs."
  (interactive)
  (let* ((f (file-truename (dired-file-name-at-point)))
         (ext (downcase (file-name-extension f)))
         (prog (alist-get ext dired-launch-programs nil nil 'equal))
         (cmd (cond
               ((stringp prog) (list prog f))
               ((functionp prog) (funcall prog f))
               ((null prog) (error "No program for file: " f)))))
    (apply 'start-process
           "*dired launch*"
           (get-buffer-create "*dired launch*")
           cmd)))

(define-keys dired-mode-map
  "<DEL>" 'dired-jump
  "i" 'dired-find-here
  "J" 'dired-launch)

(defun dired-git-here ()
  (interactive)
  (let* ((dir "/Users/shannoncole/ev/git/rmoney")
         (bname (concat "dired git - " dir))
         (buf (get-buffer-create bname)))
    (with-current-buffer buf
      (fundamental-mode)
      (erase-buffer)
      (cd dir)
      ;; (insert "  " dir ":\n")
      (shell-command "git ls-tree --name-only -r HEAD | xargs ls -l" buf)
      (dired-mode dir)
      ;; (dired-insert-set-properties (point-min) (point-max))
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker)))))
    (switch-to-buffer buf)))
