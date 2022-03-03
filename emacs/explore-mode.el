;; Global minor mode for navigation _without_ editing.
;;
;; Key-bindings should mostly cooperate with dired, which comprises much of
;; the utility of this mode.

(defvar explore-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'dired-jump)
    (define-key map (kbd "SPC") 'scroll-up-command)
    (define-key map (kbd "<backspace>") 'scroll-down-command)
    (define-key map (kbd "b") 'scroll-down-command)
    (define-key map (kbd "<") 'beginning-of-buffer)
    (define-key map (kbd ">") 'end-of-buffer)
    ;;(define-key map (kbd "<backspace>") 'scroll-down-command)
    (define-key map (kbd "l") 'recenter)
    (define-key map (kbd "s") 'isearch-forward)
    (define-key map (kbd "g") 'git-grep-symbol-at-point)
    (define-key map (kbd "G") 'git-grep-root-symbol-at-point)
    (define-key map (kbd "r") 'rgr-here)
    (define-key map (kbd "o") 'occur)
    (define-key map (kbd ".") 'next-error)
    (define-key map (kbd ",") 'previous-error)
    (define-key map (kbd "<escape>") 'explore-mode)
    (define-key map (kbd "<tab>") 'other-window)
    (define-key map (kbd "3") 'split-window-horizontally)
    (define-key map (kbd "2") 'split-window-vertically)
    (define-key map (kbd "1") 'delete-other-windows)
    (define-key map (kbd "0") 'delete-window)
    (suppress-keymap map)
    map))

(defun suppress-explore-mode-map ()
  (add-to-list 'minor-mode-overriding-map-alist
               '(explore-mode . nil)))

;; todo how to get it out of minibuffer local binding again???
;; (defun stop-suppress-explore-mode-map ()
;;   (setq minor-mode-overriding-map-alist
;;         (delete '(explore-mode)
;;                 minor-mode-overriding-map-alist)))

(setq nothing-map (make-sparse-keymap))

(define-minor-mode explore-mode
  "mode for gettin about"
  :global t
  :keymap explore-mode-map
  (if explore-mode
      (progn
        ;; Temporarily disable ourself in minibuffer so that our bindings
        ;; don't clobber basic text entry.
        ;;
        ;; TODO maybe there's a better way to achieve this?
        (add-hook 'minibuffer-setup-hook 'suppress-explore-mode-map))
    (progn
      (remove-hook 'minibuffer-setup-hook 'suppress-explore-mode-map))))

(provide 'explore-mode)
