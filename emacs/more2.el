(require 'pick nil t)
(global-set-key (kbd "<f1>") 'pick-select-buffer)
(global-set-key (kbd "<f2>") 'pick-filelist)
(pick-define-function-keys)
(pick-define-numpad-keys)

(require 'selected nil t)
(define-key selected-keymap (kbd "<return>") 'kill-ring-save)
(define-key selected-keymap (kbd "r") 'query-replace-maybe-region)
(global-set-key (kbd "<C-return>") 'yank)
(selected-global-mode)

(load "experiments/search.el")

(defun save-all () (interactive) (save-some-buffers t))

(setq-default
 mode-line-format
 '((:eval (if (get-buffer-process (current-buffer))
              '(:propertize ">>>" face (:background "orange"))
            "%*"))
   " %b:%l:%c"))


;; todo
;; modeline
;; lispy
;; stop pressing tab so much
;; switcheroo project

(defun really-kill-buffer ()
  (interactive) (kill-buffer nil))

(require 'cl)
(require 'dired-x)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(defun define-keys (keymap &rest keys)
  "Make multiple bindings in a map."
  (cl-loop for (key binding) on keys by #'cddr do
           (define-key keymap (kbd key) binding)))

(defun keymap (&rest bindings)
  "Make a new keymap with bindings. Return that map."
  (let ((map (make-sparse-keymap)))
    (apply 'define-keys map bindings)
    map))

(global-set-key (kbd "<tab>") 'other-window)

(define-keys minibuffer-local-map
  "<escape>" 'top-level)

(let ((keys (keymap
             "j" 'dired-jump
             "k" 'really-kill-buffer
             "e" 'eval-buffer
             "c" 'new-frame
             "q" 'quit
             "0" 'delete-window
             "1" 'delete-other-windows
             "2" 'split-window-below
             "3" 'split-window-right
             "f" 'find-file
             "g" 'google
             "b" 'switch-to-buffer
             "s" 'isearch-forward
             "h" (keymap "f" 'describe-function
                         "v" 'describe-variable
                         "k" 'describe-key)
             "<f12>" 'save-all
             "<escape>" 'top-level)))
  (global-set-key (kbd "<escape>") keys)
  (global-set-key (kbd "<f12>") keys))
