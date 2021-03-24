(defun quick-run (dir prog &rest args)
  (interactive)
  (save-some-buffers t)
  (let ((default-directory dir))
    (compilation-start
     (string-join (apply 'list prog args) " "))))

(defvar quick-runner-active nil
  "The currently active spec to run.")

(defun quick-runner-run ()
  (interactive)
  (unless quick-runner-active (error "No active spec."))
  (apply 'quick-run quick-runner-active))

(defun quick-runner-use-line-and-run ()
  (interactive)
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
    (setq quick-runner-active (split-string line))
    (quick-runner-run))
  (let ((buf (get-buffer "*quick*")))
    (unless (get-buffer-window buf t)
      (switch-to-buffer buf))))

(defvar quick-runner-mode-map (make-sparse-keymap))
(define-key quick-runner-mode-map (kbd "C-c C-c") 'quick-runner-use-line-and-run)

(define-derived-mode quick-runner-mode fundamental-mode "quick runner mode")

(add-to-list 'auto-mode-alist '("\\.quick\\'" . quick-runner-mode))
