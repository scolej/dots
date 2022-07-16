;;
;; Search engines
;;

(defun google (term)
  (interactive "MGoogle: ")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))

(defun teclis (term)
  (interactive "MTeclis: ")
  (browse-url
   (concat "http://teclis.com/search?q="
           (url-encode-url term))))

(defun stackoverflow (term)
  (interactive "MStack Overflow: ")
  (browse-url
   (concat "https://stackoverflow.com/search?q="
           (url-encode-url term))))

;; todo thing at point
