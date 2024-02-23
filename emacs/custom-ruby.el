(setq-default ruby-indent-level 4)

(require 'ruby-mode)
(add-hook 'ruby-mode-hook 'yas-minor-mode)
;; (remove-hook 'ruby-mode-hook 'flycheck-mode)
;; (remove-hook 'ruby-mode-hook 'flymake-mode)

;; (define-key ruby-mode-map (kbd "<tab>") 'yas-expand)

(require 'ansi-color)

;; todo a minor mode that you can repeatedly query, back/forth etc
(defun ruby-ri (arg)
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol t))
         (input (read-from-minibuffer
                 (format "Ruby help for (%s): " thing)
                 nil nil nil nil thing))
         (query (if (string-empty-p input) thing input))
         (default-directory "~"))
    (let ((buf (get-buffer-create "*ri*")))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (call-process "ri" nil buf nil "--no-pager" "--no-interactive" "--format=ansi" query)
        (ansi-color-apply-on-region (point-min) (point-max))
        (beginning-of-buffer)
        (view-mode))
      (display-buffer buf))))

;; (defun ruby-ri-gem (arg)
;;   (interactive "P")
;;   (let* ((gem-dir (or (locate-dominating-file default-directory "Gemfile")
;;                       "/Users/shannoncole/rubydoc/"))
;;          (default-directory gem-dir)
;;          (thing (thing-at-point 'symbol t))
;;          (input (read-from-minibuffer
;;                  (format "Ruby help for (%s): " thing)
;;                  nil nil nil nil thing))
;;          (query (if (string-empty-p input) thing input)))
;;     (async-shell-command
;;      (string-join (list "bundle" "exec" "ri" query) " ")
;;      (get-buffer-create "*ri*"))))

(define-key ruby-mode-map (kbd "C-c r") 'ruby-ri)
(define-key ruby-mode-map (kbd "C-c C-f") nil)
(define-key ruby-mode-map (kbd "C-M-n") nil)
(define-key ruby-mode-map (kbd "C-M-p") nil)
(define-key ruby-mode-map (kbd "<f12>") 'prettier-format)
(define-key ruby-mode-map (kbd "<backtab>") 'yas-expand)

(modify-syntax-entry ?@ "." ruby-mode-syntax-table)
(modify-syntax-entry ?: "." ruby-mode-syntax-table)

(defun ruby-customizations ()
  ;; (add-hook 'after-save-hook 'prettier-format nil t)
  (setq-local inhibit-clean-trailing-whitespace-mode t
              fill-column 100))

(add-hook 'ruby-mode-hook 'ruby-customizations)


