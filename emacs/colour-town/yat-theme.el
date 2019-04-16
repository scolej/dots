(deftheme yat)

;; FIXME Set ansi-term-color-vector.

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)
(fringe-mode nil)

(let ((bg-plain "#ffffff")
      (bg-green "#aaffaa")
      (bg-blue "#c9ddff")
      (fg-plain "#000000")
      (fg-green "#339933")

      (divider "#5182d1")
      )
  (custom-theme-set-faces
   'yat

   `(cursor ((t (:background ,divider))))
   `(region ((t (:background ,bg-blue))))
   `(default ((t (:foreground ,fg-plain))))

   ;; `(hl-line ((t (:background "#fbdac2"))))
   ;; `(match ((t (:background "#ffc851"))))
   ;; `(show-paren-match ((t (:background "#aba880"))))
   ;; `(highlight ((t (:inherit match))))

   `(success ((t (:foreground ,fg-green :background ,bg-green))))

   `(mode-line ((t (:inherit default :overline ,divider :foreground ,divider))))
   `(mode-line-inactive ((t (:inherit mode-line))))

   `(fringe ((t (:inherit default :foreground ,divider))))
   `(vertical-border ((t (:foreground ,fg-plain))))

   ;; `(header-line ((t (:inherit default :weight bold))))
   ;; `(minibuffer-prompt ((t (:inherit default))))

   ;; `(trailing-whitespace ((t (:background ,bg-red))))

   `(window-divider ((t (:inherit default :foreground ,divider))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-default))))

   ;; `(font-lock-constant-face ((t (:foreground "#90945a"))))
   ;; `(font-lock-variable-name-face ((t (:foreground "#90945a"))))
   ;; `(font-lock-function-name-face ((t (:foreground "#90945a"))))
   ;; `(font-lock-builtin-face ((t (:foreground "#3e5a70"))))
   ;; `(font-lock-keyword-face ((t (:foreground ,c61))))
   ;; `(font-lock-warning-face ((t (:inherit font-lock-keyword-face))))
   ;; `(font-lock-type-face ((t (:foreground "#245654"))))
   ;; `(font-lock-string-face ((t (:foreground ,c51))))
   ;; `(font-lock-comment-face ((t (:foreground ,c25))))

   ;; `(ivy-current-match ((t :inherit region)))
   ;; `(ivy-minibuffer-match-face-1 ((t :inherit hl-line)))
   ;; `(ivy-minibuffer-match-face-2 ((t :inherit ivy-minibuffer-match-face-1)))
   ;; `(ivy-minibuffer-match-face-3 ((t :inherit ivy-minibuffer-match-face-1)))
   ;; `(ivy-minibuffer-match-face-4 ((t :inherit ivy-minibuffer-match-face-1)))
   ;; `(ivy-minibuffer-match-highlight ((t :inherit ivy-minibuffer-match-face-1)))

   ;; `(isearch ((t (:inherit match))))
   ;; `(lazy-highlight ((t (:inherit match))))

   ;; `(dired-directory ((t (:weight bold))))
   ;; `(dired-header ((t (:inherit header-line))))

   ;; `(ag-hit-face ((t :underline t)))
   ))

(provide-theme 'yat)
