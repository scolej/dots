;;; -*- lexical-binding: t -*-

(require 'grep)

;; (grep-compute-defaults)

(setq
 grep-find-ignored-directories '(".git" "build" ".gradle")
 grep-find-ignored-files '()
 grep-save-buffers nil)

;; todo what's the difference between interactive M and s?
;; do they get different input histories?

(defun speedy-grep (pattern)
  (interactive "s")
  (rgrep pattern "*" default-directory nil))

;; Recursive grep which doesn't use find.
;; Find + Grep combo is too slow on Windows.
(defun rgr (prefix dir pattern)
  "Run grep recursively."
  (interactive "P\nDIn directory: \nsSearch for: ")
  ;; TODO suggest symbol at point
  (with-selected-frame
      ;; TODO sole window
      (if prefix (make-frame) (selected-frame))
    (let ((default-directory dir))
      (compilation-start
       (concat "grep -EHIirn '" pattern "' " dir)
       'grep-mode
       (lambda (mode)
         (concat (string-join (list "*grep*" pattern dir) " - ")))))))

(defun rgr-here (pattern)
  (interactive "sSearch for: ")
  ;; TODO re-use existing window
  (rgr nil default-directory pattern))

(defun grep-here (pattern)
  (interactive "s")
  ;; TODO re-use existing window
  (rgr nil default-directory pattern))

(defun git-grep-symbol-at-point (arg)
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol t))
         (input (read-from-minibuffer
                 (format "Git grep (%s): " thing)
                 nil nil nil nil thing))
         (pattern (if (string-empty-p input) thing input))
         (compilation-buffer-name-function
          (lambda (mode)
            (concat (string-join (list "*grep*" pattern default-directory) " - ")))))
    (compilation-start
     (concat
      ;; "GIT_PAGER=cat "
      "git grep --color=never -HIin -e '" pattern "'")
     'grep-mode)))

(defun git-grep-root-symbol-at-point ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (call-interactively 'git-grep-symbol-at-point)))

(defun grep-default-dir-symbol-at-point ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (unless (stringp symbol) (error "not a symbol"))
    (grep-here symbol)))

(global-set-key (kbd "C-x g") 'rgr)

(define-key grep-mode-map (kbd "r") 'rgr-here)

(require 'dired)
(define-key dired-mode-map (kbd "r") 'rgr-here)
