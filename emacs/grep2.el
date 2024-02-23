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


(defun grep-dwim (prefix)
  (interactive "p")
  ;; (let* ((thing (thing-at-point 'symbol t))
  ;;        (input (if (region-active-p)
  ;;                   (buffer-substring-no-properties (point) (mark))
  ;;                 (read-from-minibuffer
  ;;                  (format "grep (%s): " thing)
  ;;                  nil nil nil nil thing)))
  ;;        (regex (if (string-empty-p input) thing input))
  ;;        (dir (or (locate-dominating-file default-directory ".git") default-directory))
  ;;        (cmd (string-join (list "rg" "-in" "--no-heading" (concat "'" regex "'")) " ")))
  ;;   (rg2 cmd dir))
  (grep-dir (or (locate-dominating-file default-directory ".git") default-directory))
  )

(defun grep-here (prefix)
  (interactive "p")
  ;; (let* ((thing (thing-at-point 'symbol t))
  ;;        (input (if (region-active-p)
  ;;                   (buffer-substring-no-properties (point) (mark))
  ;;                 (read-from-minibuffer
  ;;                  (format "grep (%s): " thing)
  ;;                  nil nil nil nil thing)))
  ;;        (regex (if (string-empty-p input) thing input))
  ;;        (cmd (string-join (list "rg" "-in" "--no-heading" (concat "'" regex "'")) " ")))
  ;;   (rg2 cmd default-directory))
  (grep-dir default-directory)
  )

(defun grep-dir (dir)
  (let* ((thing (thing-at-point 'symbol t))
         (input (if (region-active-p)
                    (buffer-substring-no-properties (point) (mark))
                  (read-from-minibuffer
                   (format "grep (%s): " thing)
                   nil nil nil nil thing)))
         (regex (if (string-empty-p input) thing input))
         (cmd (string-join (list "rg" "-in" "--no-heading" "--max-columns" "300" "--sort" "path" (concat "'" regex "'")) " ")))
    (rg2 cmd dir)))

;; (defun grep-dwim (prefix)
;;   (interactive "p")
;;   (message "%s" prefix)
;;   (let* ((thing (thing-at-point 'symbol t))
;;          (input (if (region-active-p)
;;                     (buffer-substring-no-properties (point) (mark))
;;                   (read-from-minibuffer
;;                    (format "grep (%s): " thing)
;;                    nil nil nil nil thing)))
;;          (regex (if (string-empty-p input) thing input))
;;          (default-directory (if (< 1 prefix) (read-directory-name "dir: ")
;;                               (or (locate-dominating-file default-directory ".git")
;;                                   default-directory)))
;;          (cmd (string-join (list "rg" "-in" "--no-heading" (concat "'" regex "'")) " ")))
;;     (when regex (rg2 (if (eq prefix 16)
;;                          (read-from-minibuffer
;;                           (format "command: ") cmd)
;;                        cmd)))))

;; (defun grep-dwim (prefix)
;;   (interactive "p")
;;   (message "%s" prefix)
;;   (let* ((thing (thing-at-point 'symbol t))
;;          (input (if (region-active-p)
;;                     (buffer-substring-no-properties (point) (mark))
;;                   (read-from-minibuffer
;;                    (format "grep (%s): " thing)
;;                    nil nil nil nil thing)))
;;          (regex (if (string-empty-p input) thing input))
;;          (default-directory (if (< 1 prefix) (read-directory-name "dir: ")
;;                               (or (locate-dominating-file default-directory ".git")
;;                                   default-directory)))
;;          (cmd (string-join (list "rg" "-in" "--no-heading" (concat "'" regex "'")) " ")))
;;     (when regex (rg2 (if (eq prefix 16)
;;                          (read-from-minibuffer
;;                           (format "command: ") cmd)
;;                        cmd)))))

(defun rg2 (cmd dir)
  (interactive "M")
  (let* ((default-directory dir)
         (bufname (concat "*grep* " default-directory " " cmd))
         (buf (get-buffer-create bufname)))
    (compilation-start cmd 'grep-mode (lambda (mode) bufname))))

;; (git-grep "concat")

(global-set-key (kbd "s-g") 'grep-dwim)

;;
;;
;;
;;
;; S-g - one base "dwim" command - it either uses the region or prompts for a string
;;
;; once you've got a search
;; - d starts a new search but in a different dir
;; - f changes the files ...
;; - e edit the command string
;; but you always get a new search in a new buffer
