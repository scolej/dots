(defun grep-launchpad-launch-current-line ()
  (interactive)
  (let ((cmd (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (compilation-start cmd 'grep-mode (lambda (mode) (concat "*grepper* " cmd)))))

(defun grep-launchpad-jump-last ()
  (interactive)
  (switch-to-buffer
   (or (seq-find (lambda (buf) (equal 'grep-launchpad-mode (buffer-local-value 'major-mode buf)))
                 (buffer-list))
       (error "no recent grep launchpad"))))

(defvar grep-launchpad-mode-map (make-sparse-keymap))
(define-key grep-launchpad-mode-map (kbd "C-c C-c") 'grep-launchpad-launch-current-line)

(define-derived-mode grep-launchpad-mode
  fundamental-mode "grep launchpad mode")


