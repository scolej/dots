;;; -*- lexical-binding: t -*-

(define-derived-mode new-grep-mode grep-mode "new grep mode"
  :keymap new-grep-mode-map)

(defvar-local ng-regex nil)
(defvar-local ng-dir nil)
(defvar-local ng-ignore-case nil)

(defvar new-grep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'new-grep-repeat-with-dir)
    (define-key map (kbd "r") 'new-grep-repeat-with-regex)
    (define-key map (kbd "i") 'new-grep-repeat-toggle-case)
    map))

(defun rg3 (dir regex ignore-case)
  (let* ((default-directory dir)
         (bufname (concat "rg " dir " " regex (if ignore-case " ignore-case" "")))
         (buf (get-buffer-create bufname)))
    (let ((cmd (string-join (list "rg" "-n" "--no-heading"
                                  "--max-columns" "300" "--sort" "path"
                                  (if ignore-case "-i" "")
                                  (concat "'" regex "'"))
                            " ")))
      (let ((display-buffer-alist '((t . (display-buffer-same-window)))))
        (compilation-start cmd 'new-grep-mode (lambda (mode) bufname)))
      (with-current-buffer buf
        (setq-local ng-regex regex
                    ng-dir dir
                    ng-ignore-case ignore-case)))))

;;;

(defun grep-dir (dir)
  (let* ((thing (thing-at-point 'symbol t))
         (input (if (region-active-p)
                    (buffer-substring-no-properties (point) (mark))
                  (read-from-minibuffer
                   (format "grep (%s): " thing)
                   nil nil nil nil thing)))
         (regex (if (string-empty-p input) thing input)))
    (rg3 dir regex t)))

(defun grep-dwim (prefix)
  (interactive "p")
  (grep-dir (or (locate-dominating-file default-directory ".git") default-directory)))

(defun grep-here (prefix)
  (interactive "p")
  (grep-dir default-directory))

(defun new-grep-repeat-with-dir (dir) (interactive "DSearch Dir: ")
       (let ((display-buffer-alist '((t . (display-buffer-same-window)))))
         (rg3 dir ng-regex ng-ignore-case)))
(defun new-grep-repeat-with-regex (regex) (interactive "MRegex: ") (rg3 ng-dir regex ng-ignore-case))
(defun new-grep-repeat-toggle-case () (interactive) (rg3 ng-dir ng-regex (if ng-ignore-case nil t)))

(global-set-key (kbd "s-g") 'grep-dwim)
