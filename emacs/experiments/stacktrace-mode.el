;;; -*- lexical-binding: t -*-

;; todo
;;
;; handle changes and re-run on changed portions of buffer
;;
;; could also have "likely roots" instead of default-dir; first try monorepo
;; root, then try root/web-client ...
;;
;; don't use buttons, use text-properties? can then do bindings like dired:
;; open in other window without selecting, next line prev line etc

(define-derived-mode stacktrace-mode
  fundamental-mode "stacktrace mode"
  (add-hook 'after-revert-hook 'stacktrace-add-buttons nil t)
  (stacktrace-add-buttons))

(defun maybe-string-remove-prefix (prefix string)
  "Remove a prefix from a string, but return nil if there's no
matching prefix to remove."
  (when (string-prefix-p prefix string)
    (string-remove-prefix prefix string)))

(defun strip-app-prefix (path) (maybe-string-remove-prefix "/app/" path))

(defvar stacktrace-path-transformers
  '(strip-app-prefix))

(defun find-apply (funs val)
  "Apply each of a list of functions to a value in turn, but stop
and return the first non-nil result."
  (catch 'found
    (dolist (fun funs)
      (let ((v (apply fun val nil)))
        (when v (throw 'found v))))
    val))

(defun stacktrace-add-buttons ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\\([-_/.[:alnum:]]+\\):\\([[:digit:]]+\\)" nil t)
      (let* ((path (match-string 1))
             (linum (string-to-number (match-string 2)))
             (path1 (find-apply stacktrace-path-transformers path))
             (full-path (if (file-name-absolute-p path1) path1
                          (file-name-concat default-directory path1))))
        (when (file-exists-p full-path)
          (make-text-button
           (match-beginning 0) (match-end 0)
           'action (lambda (unused)
                     (find-file-other-window full-path)
                     (goto-line linum))))))))

(defun yank-stacktrace ()
  (interactive)
  (save-excursion
    (when (region-active-p) (delete-region (point) (mark)))
    (yank)
    (stacktrace-add-buttons)))

;; or maybe just: file name and number in git repo mode
;;
;; start with file name, link if its unique in repo
;; add parent dirs until unique

(provide 'stacktrace-mode)
