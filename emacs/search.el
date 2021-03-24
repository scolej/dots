(defun expand-engine (abbrev)
  (pcase abbrev
    ("g" 'google)
    ("d" 'duck-duck-go)
    ("w" 'wikipedia)))

(defun find-engine-on-line ()
  "Finds the first word on a line. Space separated."
  (save-excursion
    (beginning-of-line)
    (search-forward " " (point-at-eol) t))
  (buffer-substring-no-properties (point-at-bol) (match-beginning 0)))

(defun find-query-on-line ()
  "Finds the query on this line: everything after the first space."
  (save-excursion
    (beginning-of-line)
    (search-forward " " (point-at-eol) t))
  (buffer-substring-no-properties (match-end 0) (point-at-eol)))

(defun make-url-for-engine (engine query)
  (let ((q (url-encode-url query)))
    (pcase engine
      ('google
       (format "https://google.com/search?query=%s" q))
      ('duck-duck-go
       (format "https://html.duckduckgo.com/html?q=%s" q)))))

(defun search-current-line ()
  (interactive)
  (let* ((engine (expand-engine (find-engine-on-line)))
         (query (find-query-on-line))
         (url (make-url-for-engine engine query)))
    (browse-url url)))

(defvar search-engine-mode-map (make-sparse-keymap))
(define-key search-engine-mode-map (kbd "C-c C-c") 'search-current-line)
(define-key search-engine-mode-map (kbd "<escape> <return>") 'search-current-line)

(define-derived-mode search-engine-mode fundamental-mode "search engine mode")
