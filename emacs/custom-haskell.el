;;
;; Configuration for Haskell mode
;;

;; (setq haskell-interactive-popup-errors nil)

;; (require 'format-all)
;; (add-hook 'haskell-mode-hook 'format-all-mode)
;; (add-hook 'haskell-mode-hook 'format-all-ensure-formatter)

;; probably doesn't belong here...
;; (add-to-list 'format-all-default-formatters '("Haskell" fourmolu))


;; ------------------

;; (defun sane-indent-setup ()
;;   (define-key haskell-mode-map (kbd "<backtab>") 'simple-backtab)
;;   (define-key haskell-mode-map (kbd "<tab>") 'simple-tab)
;;   (haskell-indent-mode -1)
;;   (haskell-indentation-mode -1)
;;   (setq tab-width 2))

;; (remove-hook 'haskell-mode-hook 'sane-indent-setup)

;; todo needs to work on region

;; todo don't need a loop

;; (defun simple-tab ()
;;   (interactive)
;;   (save-excursion
;;     (back-to-indentation)
;;     (insert " ")
;;     (let ((b (point-at-bol)))
;;       (while (not (equal 0 (mod (- (point) b) tab-width)))
;;         (insert " ")))))

;; (defun simple-backtab ()
;;   (interactive)
;;   (save-excursion
;;     (back-to-indentation)
;;     (delete-backward-char 1)
;;     (let ((b (point-at-bol)))
;;       (while (not (equal 0 (mod (- (point) b) tab-width)))
;;         (delete-backward-char 1)))))

;; function for position at start of line

;; todo
;;
;; - pressing M-j should preserve space after comment
;;
;; - M-j should work in haskell-mode

(require 'haskell-mode)

;; (require 'stupid-indent-mode)
;; (define-keys haskell-mode-map
;;              "<tab>" 'stupid-indent
;;              "<backtab>" 'stupid-deindent
;;              "<S-tab>" 'stupid-deindent
;;              )

(define-keys haskell-indentation-mode-map
             ";" nil)
