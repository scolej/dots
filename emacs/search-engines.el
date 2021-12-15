;;
;; Search engines
;;

(defun google (term)
  (interactive "MGoogle: ")
  (browse-url
   (concat "https://google.com/search?query="
           (url-encode-url term))))

;; todo thing at point
