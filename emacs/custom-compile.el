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

(defun guess-compilation-command (dir filename)
  (let ((contents (directory-files dir))
        (dir (expand-file-name dir)))
    (cond
     ((string-suffix-p ".sh" filename) (concat "sh " filename))
     ((string-suffix-p ".rb" filename) (concat "ruby " filename))
     ((seq-contains contents "Cargo.toml") "cargo fmt && cargo build")
     ((seq-contains contents "timelog.scm") "GUILE_LOAD_PATH=/Users/shannoncole/ev/hours2 guile timelog.scm")
     ((string-suffix-p ".hs" (buffer-file-name)) (concat "runhaskell " (buffer-file-name)))
     ((equal dir "/Users/shannoncole/rubyscratch/") "rub 02.rb")
     (nil))))

(defun compile-in-dir (dir cmd)
  (interactive
   (let* ((dir (read-directory-name "Compilation dir: "))
          (cmd (read-string "Command: "
                            (guess-compilation-command dir (when (buffer-file-name) (file-relative-name (buffer-file-name) dir)))
                            'compilation-command-history)))
     (list dir cmd)))
  (let ((default-directory dir))
    (compile cmd)))

;; (defun dwim-compile-action ()
;;   (cond ()))

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
  (let ((display-buffer-overriding-action '(display-buffer-actually-no . nil)))
    (recompile)))

(gsk "<f11>" 'recompile)
(gsk "<kp-add>" 'next-error)
(gsk "<kp-subtract>" 'previous-error)

;; (defun use-smaller-text () (text-scale-set -1))
;; (add-hook 'compilation-mode-hook 'use-smaller-text)

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

(defun tickle-dominating-trigger ()
  (interactive)
  (save-all)
  (let ((default-directory
         (or (locate-dominating-file default-directory "trigger")
             (error "Couldn't find dominating trigger file, can you make it?"))))
    (shell-command "date > trigger")
    (message "Tickled trigger in %s" default-directory)))

(gsk "<f10>" 'tickle-dominating-trigger)
