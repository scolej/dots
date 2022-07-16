;;
;; Compilation
;;

;; todo
;;
;; different compilation buffers associated with differents dir roots?
;;
;; eg i'm in rubyscratch, when i press f11 i want to do the compile action
;; from this dir. then i go to a previously opened rust project; f11 should
;; work there too.

(defun guess-compilation-command (dir)
  (let ((contents (directory-files dir))
        (dir (expand-file-name dir)))
    (cond
     ((seq-contains contents "Cargo.toml") "cargo build")
     ((seq-contains contents "timelog.scm") "GUILE_LOAD_PATH=/Users/shannoncole/ev/hours2 guile timelog.scm")
     ((equal dir "/Users/shannoncole/rubyscratch/") "rub 02.rb")
     (nil))))

(defun compile-in-dir (dir cmd)
  (interactive
   (let* ((dir (read-directory-name "Compilation dir: "))
          (cmd (read-string "Command: " (guess-compilation-command dir) 'compilation-command-history)))
     (let ((default-directory dir)
           (compilation-scroll-output 'first-error))
       (compile cmd)))))

(defun compilation-buffer-p (b)
  "Is the given buffer in a mode derived from compilation mode?"
  (with-current-buffer b
    (or (eq major-mode 'comint-mode)
        (derived-mode-p 'compilation-mode))))

(defun find-recent-compilation-buffer ()
  "Finds the most recent buffer which is a compilation mode. (Or
the current buffer, if it's a compilation mode!)"
  (let ((b (current-buffer)))
    (if (compilation-buffer-p b) b
      (seq-find 'compilation-buffer-p
                (buffer-list (selected-frame))))))

(defun recompile-recent-compilation (arg)
  (interactive "P")
  (save-some-buffers t)
  (let ((b (find-recent-compilation-buffer)))
    (if b (with-current-buffer b
            (if arg (call-interactively 'recompile)
              (recompile)))
      (call-interactively 'compile))))

(setq-default compilation-always-kill t
              compilation-ask-about-save nil)

;; FIXME does nothing?
(add-hook 'compilation-mode-hook
          'ansi-color-for-comint-mode-on)

(setq compilation-always-kill t
      compilation-mode-font-lock-keywords nil
      compilation-search-path '("." "src"))

;; Suitable for use in display-buffer-overriding-action to inhibit buffer display.
(defun display-buffer-actually-no (&rest args) t)

;; Recompile without showing the compilation buffer.
;; I'll find it myself thank you very much.
(defun recompile-inhibit-buffer ()
  (interactive)
  (let ((display-buffer-overriding-action '(display-buffer-actually-no . nil))
        (compilation-scroll-output 'first-error))
    (recompile)))

(gsk "<f11>" 'recompile-inhibit-buffer)
(gsk "<kp-add>" 'next-error)
(gsk "<kp-subtract>" 'previous-error)

(defun use-smaller-text () (text-scale-set -1))
;; (remove-hook 'compilation-mode-hook 'use-smaller-text)

(defun compile-date-trigger-in-git-root ()
  "Find the Git root and start a compilation buffer which just
echoes the current date into a trigger file. Handy for saving
everything and kicking off an external process which can wait on
the trigger file."
  (interactive)
  (let ((display-buffer-overriding-action '(display-buffer-actually-no . nil))
        (default-directory
          (locate-dominating-file default-directory ".git")))
    (compile "date > trigger")))
