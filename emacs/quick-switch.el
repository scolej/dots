(defvar quick-switch-buffers '())

(defun quick-switch-put-buffer (reg)
  (setf (alist-get reg quick-switch-buffers) (current-buffer)))

(defun quick-switch-jump (reg)
  (let ((display-buffer-alist '((".*" . ((display-buffer-reuse-window
                                          display-buffer-same-window) . nil)))))
    (pop-to-buffer
     (alist-get reg quick-switch-buffers))))

(define-minor-mode quick-switch
  "Buffer switching keys."
  :global t
  :keymap
  (let ((km (make-sparse-keymap)))
    (dolist (c '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
      (define-key km (kbd (format "C-M-%c" c))
        `(lambda () (interactive) (quick-switch-put-buffer ,c)))
      (let ((jump `(lambda () (interactive) (quick-switch-jump ,c))))
        (define-key km (kbd (format "M-%c" c)) jump)))
    km))



;; todo lexical bind instead of quote lambda?

;;;

;; todo disable in minibuffer

;; (defun hide-map ()
;;   (setq-local quick-switch-map nil))
;; (add-hook 'minibuffer-setup-hook 'hide-map)

;; turn mode on and off

;; add kp-bindings to minibuffer-local-map

;; minor-mode-overriding-map-alist



