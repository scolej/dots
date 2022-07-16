;; Ideas
;;
;; Add associations between places and roots. ie: if I'm in directory A, jump to files in directory B.
;; hopper-dominating-directories
;;     A -> B
;;     C -> D
;; or hopper-determine-root
;; a function to decide what to use as the root

(defvar hopper-root nil)

(defun hopper-cd (dir)
  "Change the hopper directory."
  (interactive "DNew hopper dir: ")
  (setq hopper-root dir))

(defun hop-test-file (dir file)
  "Does FILE exist at DIR? If so, return it."
  (let ((f (concat (file-name-as-directory dir) file)))
    (if (file-exists-p f) f
      nil)))

(defun hop-to-file (file &optional line column)
  (let ((target (or (if (file-exists-p file) file nil)
                    (hop-test-file default-directory file)
                    (hop-test-file hopper-root file))))
    (if (not target) (call-interactively 'find-file)
      (find-file target)
      (when line (goto-line line))
      (when column (move-to-column column))
      (recenter))))

(defconst hopper-not-path-char-regex "[^[:alnum:]-_./:\\]"
  "Regexp matching characters which should mark the bounds of a file path.")

(defun hopper-file-string-at-point ()
  "Look at what's around point and return a likely path string."
  (buffer-substring-no-properties
   (or (when (save-excursion
               (re-search-backward hopper-not-path-char-regex (point-at-bol) t))
         (1+ (match-beginning 0)))
       (point-at-bol))
   (or (when (save-excursion
               (re-search-forward hopper-not-path-char-regex (point-at-eol) t))
         (1- (match-end 0)))
       (point-at-eol))))

;; todo
;;
;; attempt to resolve path relative to current git root
;;
;; attempt to resolve path as a substring of tracked files in git tree

;; FIXME its own history
(defun file-hopper ()
  "Look at the path at point, and try to jump to an appropriate file."
  (interactive)
  (unless hopper-root (error "No hopper root defined. Try `hopper-cd`."))
  (let ((str (hopper-file-string-at-point)))
    (cond
     ((string-empty-p str)
      (call-interactively 'find-file))
     ((string-match "\\([^:]*\\):\\([[:digit:]]+\\)" str)
      ;; TODO match line & column
      (let ((path (match-string 1 str))
            (line (string-to-number (match-string 2 str))))
        (hop-to-file path line)))
     (t
      (hop-to-file str)))))

(global-set-key (kbd "C-x f") 'file-hopper)
;; (global-set-key (kbd "C-x C-f") 'find-file)
