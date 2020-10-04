(require 'grep)

;; (grep-compute-defaults)

(setq grep-find-ignored-directories '(".git" "build" ".gradle")
      grep-find-ignored-files '()
      grep-save-buffers nil)

(defun speedy-grep (pattern)
  (interactive "M")
  (rgrep pattern "*" default-directory nil))

;; Recursive grep which doesn't use find.
;; Find + Grep combo is too slow on Windows.
(defun rgr (pattern dir)
  "Run grep recursively."
  (interactive "MSearch for: \nDIn directory: ")
  (compilation-start
   (concat "grep -Hirn " pattern " " dir)
   'grep-mode))

(defun git-grep-symbol-at-point (arg)
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol t))
         (input (read-from-minibuffer
                 (format "Git grep (%s): " thing)
                 nil nil nil nil thing))
         (pattern (if (string-empty-p input) thing input)))
    (compilation-start
     (concat
      ;; "GIT_PAGER=cat "
      "git grep --color=never -HIin -e '" pattern "'")
     'grep-mode)))

(defun git-grep-root-symbol-at-point ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (call-interactively 'git-grep-symbol-at-point)))

(global-set-key (kbd "C-x g") 'git-grep-symbol-at-point)
(global-set-key (kbd "C-x G") 'git-grep-root-symbol-at-point)
