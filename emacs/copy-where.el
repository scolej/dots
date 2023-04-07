(defun copy-path-git ()
  "Copy the git-relative path to the current file."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (error "not in a git repo")))
         (abs (or (buffer-file-name)
                  (default-directory)))
         (str (file-relative-name abs root)))
    (kill-new str)
    (let ((x-select-enable-primary t))
      (x-select-text str))
    (message (format "Copied: %s" str))))

;; TODO there's some factoring here for sure
;; TODO if there's an active region, use the whole region and correctly indent the snippet
(defun copy-crumb ()
  "Copy file path, line number, and trimmed line."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (error "not in a git repo")))
         (abs (or (buffer-file-name)
                  (default-directory)))
         (line (s-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
         (linum (number-to-string (line-number-at-pos (point))))
         (str1 (concat (file-relative-name abs root) ":" linum " " line))
         (str2 (concat str1 "\n")))
    (kill-new str2)
    (let ((x-select-enable-primary t))
      (x-select-text str2))
    ;; todo crumbs with %s in them !?
    (message (format "Copied crumb: %s" str1))))

(defun copy-git-buffer-path ()
  "Copy the Git-root-relative path to the current buffer's file."
  (interactive)
  (let* ((git-root (or (locate-dominating-file default-directory ".git") (error "not in a git repo")))
         (full-path (or (buffer-file-name) default-directory))
         (str (file-relative-name full-path git-root)))
    (kill-new str)
    (message (format "Copied: %s" str))))

;; todo should these be git-aware?
(defun copy-buffer-path ()
  "Copy the full path to the current buffer's file."
  (interactive)
  (let ((str (cond (buffer-file-name)
                   (default-directory))))
    (kill-new str)
    (message (format "Copied: %s" str))))

(defun copy-buffer-path-and-line ()
  "Copy the full path to the current buffer's file and append a
colon followed by the line number."
  (interactive)
  (let ((s (concat (buffer-file-name)
                   ":"
                   (number-to-string (line-number-at-pos (point))))))
    (kill-new s)
    (message (format "Copied: %s" s))))

