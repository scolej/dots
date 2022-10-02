;; (defun terraform-format-buffer ()
;;   (interactive)
;;   (call-process-buffer-replace "terraform" "fmt" "-"))

(define-key terraform-mode-map (kbd "C-c C-f") 'terraform-format-buffer)
