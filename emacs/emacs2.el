;; (require 'package)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-initialize)

(setq inhibit-splash-screen t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode -1)
(blink-cursor-mode -1)
(show-paren-mode 1)

(setq auto-save-timeout 20)

(global-set-key (kbd "C-z") 'undo)

(defun kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer))
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(setf ansi-color-for-comint-mode 'filter)
