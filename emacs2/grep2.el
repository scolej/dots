;;; -*- lexical-binding: t -*-

(defvar new-grep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'new-grep-repeat-with-dir)
    (define-key map (kbd "r") 'new-grep-repeat-with-regex)
    (define-key map (kbd "i") 'new-grep-repeat-toggle-case)
    (define-key map (kbd "f") 'new-grep-repeat-with-file-glob)
    map))

(define-derived-mode new-grep-mode grep-mode "new grep mode"
  :keymap new-grep-mode-map)

(defvar-local ng-regex nil)
(defvar-local ng-dir nil)
(defvar-local ng-ignore-case nil)
(defvar-local ng-file-glob nil)

;; todo
;; use c to toggle case
;; use i to edit a string to pass to --iglob
;; see how rg mode does the multi-line file name thing and do that
;; 
;; a variant which uses project to search the current project
;; this would be good for monorepo with lots of projects in it 

(defun rg3 (dir regex ignore-case file-glob)
  (let ((display-buffer-alist '((t . (display-buffer-same-window))))
        (default-directory dir)
        (cmd (string-join (list "rg" "-n" "--no-heading"
                                "--max-columns" "300" "--sort" "path"
                                (if file-glob (concat "-g '" file-glob "'") "")
                                (if ignore-case "-i" "")
                                "--"
                                (concat "'" (string-replace "'" "'\\''" regex) "'"))
                          " ")))
    (with-current-buffer
        (compilation-start cmd 'new-grep-mode
                           (lambda (mode)
                             (concat "rg " dir " " regex
                                     (if ignore-case " ignore-case" "")
                                     (if file-glob (concat " files: " file-glob) ""))))
      (setq-local ng-regex regex
                  ng-dir dir
                  ng-ignore-case ignore-case
                  ng-file-glob file-glob))))

;;;

(defun grep-dir (dir)
  (let* ((thing (or (thing-at-point 'symbol t)
                    (current-kill 0)))
         (input (if (region-active-p)
                    (buffer-substring-no-properties (point) (mark))
                  (read-from-minibuffer
                   (format "grep (%s): " thing)
                   nil nil nil nil thing)))
         (regex (if (string-empty-p input) thing input)))
    (rg3 dir regex t nil)))

(defun grep-dwim (prefix)
  (interactive "p")
  (grep-dir default-directory))

(defun grep-git-root (prefix)
  (interactive "p")
  (grep-dir (or (locate-dominating-file default-directory ".git") (error "not a git repo"))))

(defun new-grep-repeat-with-dir (dir)
  (interactive "DSearch Dir: ")
  (let ((display-buffer-alist '((t . (display-buffer-same-window)))))
    (rg3 dir ng-regex ng-ignore-case ng-file-glob)))

(defun new-grep-repeat-with-regex (regex)
  (interactive "MRegex: ")
  (rg3 ng-dir regex ng-ignore-case ng-file-glob))

(defun new-grep-repeat-toggle-case ()
  (interactive)
  (rg3 ng-dir ng-regex (if ng-ignore-case nil t) ng-file-glob))

(defun new-grep-repeat-with-file-glob (file-glob)
  (interactive "MGlob: ")
  (rg3 ng-dir ng-regex ng-ignore-case file-glob))

(global-set-key (kbd "s-g") 'grep-dwim)
(global-set-key (kbd "s-G") 'grep-git-root)
