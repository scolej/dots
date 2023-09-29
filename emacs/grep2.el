;;; -*- lexical-binding: t -*-

;; grep / rg
;; use custom buffer name for each search
;; easily repeat / customize a search

;; (require 'grep)

;; (defun search (regex &optional include exclude)
;;   (interactive "M")
;;   (let* ((cmd (string-join (append '("git" "grep" "--name-only" "-r" "HEAD")
;;                                    (when include (list "|" "grep" "-i" include))
;;                                    (list "|" "xargs" "-d" "\\n" "grep" "-Hin" regex))
;;                            " "))
;;          ;; (cmd (string-join (append '("git" "ls-tree" "--name-only" "-r" "HEAD")
;;          ;;                           (when include (list "|" "grep" "-i" include))
;;          ;;                           (list "|" "xargs" "-d" "\\n" "grep" "-Hin" regex))
;;          ;;                   " "))
;;          ;; (cmd (string-join (append '("find" "." "-type" "f")
;;          ;;                           (when include (list "-iname" include))
;;          ;;                           (list "|" "xargs" "grep" "-Hin" regex))
;;          ;;                   " "))
;;          (bufname (concat "search - " default-directory " - " cmd))
;;          (buf (get-buffer-create bufname)))
;;     (compilation-start cmd 'grep-mode (lambda (mode) bufname))))

;; (defun git-grep-dwim (prefix)
;;   (interactive "P")
;;   (let* ((thing (thing-at-point 'symbol t))
;;          (guess (cond
;;                  ((region-active-p) (buffer-substring-no-properties (point) (mark)))
;;                  (thing thing)))
;;          (input (read-from-minibuffer
;;                  (format "Git grep (%s): " guess)
;;                  nil nil nil nil guess))
;;          (regex (if (string-empty-p input) guess input)))
;;     (git-grep regex)))

(defun git-grep-dwim (prefix)
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol t))
         (input (if (region-active-p)
                    (buffer-substring-no-properties (point) (mark))
                  (read-from-minibuffer
                   (format "Git grep (%s): " thing)
                   nil nil nil nil thing)))
         (regex (if (string-empty-p input) thing input))
         (cmd (string-join (list "git" "grep" "-Iin" (concat "'" regex "'")) " ")))
    (when regex (git-grep (if prefix
                              (read-from-minibuffer
                               (format "Grep command: ") cmd)
                            cmd)))))

(defun git-grep (cmd)
  (interactive "M")
  (let* ((default-directory (or (locate-dominating-file default-directory ".git") (error "not in a git repo")))
         (bufname (concat "*grep* " default-directory " " cmd))
         (buf (get-buffer-create bufname)))
    (compilation-start cmd 'grep-mode (lambda (mode) bufname))))

;; (git-grep "concat")

(global-set-key (kbd "s-g") 'git-grep-dwim)
