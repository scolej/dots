(setq-default ruby-indent-level 4)

(require 'ruby-mode)
(add-hook 'ruby-mode-hook 'yas-minor-mode)

;; (define-key ruby-mode-map (kbd "<tab>") 'yas-expand)

(defun ruby-ri (arg)
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol t))
         (input (read-from-minibuffer
                 (format "Ruby help for (%s): " thing)
                 nil nil nil nil thing))
         (query (if (string-empty-p input) thing input)))
    (async-shell-command
     (string-join (list "ri" query) " ")
     (get-buffer-create "*ri*"))))

(define-key ruby-mode-map (kbd "C-c r") 'ruby-ri)

(modify-syntax-entry ?@ "_" ruby-mode-syntax-table)