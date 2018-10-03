;; TODO
;; Maybe headers which are URLs should automatically be converted to "site:// ..." Google searches

(require 'subr-x)

(defun find-context ()
  "Get the last line which started with a non-blank character."
  (save-excursion
    (re-search-backward "^[^[:space:]].*$")
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun current-line ()
  (string-trim (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun char-is-whitespace (c)
  (= 32 (char-syntax c)))

(defun search-this-line-in-context ()
  (interactive)
  (if (not (char-is-whitespace (char-after (point-at-bol))))
      (message "Line is not a command! (Needs to be indented.)")
    (progn
      (let* ((query (current-line))
             (engine (find-context)))
        (unless (string-empty-p query)
          (do-search query engine))))))

(defun googly-new-command ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(defun do-search (q e)
  (browse-url
   (pcase e
     ("google"
      (concat "https://google.com/search?query="
              (url-encode-url q)))
     ("wiki"
      (concat "https://en.wikipedia.org/w/index.php?search="
              (url-encode-url q)))
     ("gradledoc"
      (concat "https://google.com/search?query="
              (url-encode-url (string-join (list "site:https://docs.gradle.org/current/" q) " "))))
     ("groovy doc"
      (concat "https://google.com/search?query="
              (url-encode-url
               (string-join (list "site:http://docs.groovy-lang.org/docs/latest/html/" q) " "))))
     (_ (error (string-join (list "Unknown engine: " e)))))))

(defvar le-googly-mode-map (make-sparse-keymap))
(define-key le-googly-mode-map (kbd "<return>") 'search-this-line-in-context)
(define-key le-googly-mode-map (kbd "<S-return>") 'googly-new-command)

(defvar le-googly-highlights '(("^[^[:space:]].*$" . font-lock-function-name-face)))

(defun le-googly-indent ()
  (indent-line-to (pcase (current-indentation)
                    (0 1) (_ 0))))

(define-derived-mode le-googly-mode fundamental-mode " g"
  (setq-local indent-line-function #'le-googly-indent)
  (setq font-lock-defaults '(le-googly-highlights)))

(add-to-list 'auto-mode-alist '("\\.googly\\'" . le-googly-mode))

(provide 'le-googly)
