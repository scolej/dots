(defun terraform-format-buffer ()
  (interactive)
  (call-process-buffer-replace "terraform" "fmt" "-"))
