(defun isearch-use-region (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (when (use-region-p)
    (let ((search (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))))
      (setq deactivate-mark t)
      (isearch-yank-string search))))

;; means you can't mark region and then start searching :(
;; (advice-add 'isearch-forward :after 'isearch-use-region)
;; (advice-add 'isearch-backward :after 'isearch-use-region)

(setq isearch-allow-scroll t
      isearch-wrap-function '(lambda nil)
      isearch-yank-on-move nil)
