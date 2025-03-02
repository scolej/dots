(defvar compilation-file-mode-map (make-sparse-keymap))
(define-key compilation-file-mode-map (kbd "<f5>") 'compilation-file-refresh)

(defun compilation-file-refresh ()
  (interactive)
  (revert-buffer t t)
  (read-only-mode -1)
  (display-ansi-colors)
  (read-only-mode 1)
  (compilation-file-mode))

(define-derived-mode compilation-file-mode compilation-mode "compilation file mode"
  (setq-local compilation-parse-errors-filename-function 'add-publicapi2-prefix))

(defun add-publicapi2-prefix (file) (file-name-concat "backend/public-api2/" file))

(setq-local compilation-parse-errors-filename-function add-publicapi2-prefix)
