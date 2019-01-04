(deftheme archrome)

(let ((bg "#f8e6d4")

      (c11 "#f7dac1")
      (c12 "#ffbcae")
      (c13 "#f67c78")
      (c14 "#f85757")
      (c15 "#c93f4e")
      (c16 "#9b3d43")
      (c17 "#7b454a")
      (c18 "#7e3742")

      (c21 "#ecc39c")
      (c22 "#e6cdb2")
      (c23 "#b39878")
      (c24 "#896251")
      (c25 "#6a574b")
      (c26 "#ffe49f")
      (c27 "#e9d15b")
      (c28 "#ffc851")

      (c31 "#ddccb0")
      (c32 "#f0cfa5")
      (c33 "#d4b254")
      (c34 "#a59985")
      (c35 "#8e836f")
      (c36 "#91955c")
      (c37 "#746b51")
      (c38 "#d4e2bb")

      (c41 "#c5dcbc")
      (c42 "#dedd6a")
      (c43 "#cfd888")
      (c44 "#9cb84d")
      (c45 "#9a9e7f")
      (c46 "#728879")
      (c47 "#617356")
      (c48 "#4a574e")

      (c51 "#225553")
      (c52 "#8ca9a1")
      (c53 "#5c828f")
      (c54 "#d6e1cb")
      (c55 "#8bbac4")
      (c56 "#719fae")
      (c57 "#3c5a6e")
      (c58 "#20608e")

      (c61 "#245275")
      (c62 "#ece2cb")
      (c63 "#ccc8be")
      (c64 "#aeaaa0")
      (c65 "#93948f")
      (c66 "#757a78")
      (c67 "#616161")
      (c68 "#fae3cc"))
  (custom-theme-set-faces
   'archrome

   `(cursor ((t (:background ,c15))))
   `(region ((t (:background ,c12))))
   `(default ((t (:foreground ,c18 :background ,bg))))
   `(hl-line ((t (:background "#fbdac2"))))
   `(match ((t (:background "#ffc851"))))
   `(highlight ((t (:inherit match))))

   `(success ((t (:foreground ,c47))))

   `(mode-line ((t (:inherit default :background ,c22))))
   `(mode-line-inactive ((t (:inherit mode-line))))

   `(trailing-whitespace ((t (:background "#febbad"))))

   `(show-paren-match ((t (:background "#aba880"))))

   `(linum ((t (:inherit default :foreground "#b29777"))))

   `(window-divider ((t (:inherit default))))
   `(window-divider-first-pixel ((t (:inherit default))))
   `(window-divider-last-pixel ((t (:inherit default))))

   `(link ((t (:foreground "#3e5a70"))))
   `(link-visited ((t (:inherit link))))

   `(font-lock-constant-face ((t (:foreground "#90945a"))))
   `(font-lock-variable-name-face ((t (:foreground "#90945a"))))
   `(font-lock-function-name-face ((t (:foreground "#90945a"))))
   `(font-lock-builtin-face ((t (:foreground "#3e5a70"))))
   `(font-lock-keyword-face ((t (:foreground ,c61))))
   `(font-lock-warning-face ((t (:inherit font-lock-keyword-face))))
   `(font-lock-type-face ((t (:foreground "#245654"))))
   `(font-lock-string-face ((t (:foreground ,c51))))
   `(font-lock-comment-face ((t (:foreground ,c25))))

   `(fringe ((t (:inherit default))))
   `(vertical-border ((t (:inherit default))))

   `(header-line ((t (:inherit default :weight bold))))
   `(minibuffer-prompt ((t (:inherit default))))

   ;; `(ivy-current-match ((t :inherit region)))
   ;; `(ivy-minibuffer-match-face-1 ((t :inherit hl-line)))
   ;; `(ivy-minibuffer-match-face-2 ((t :inherit ivy-minibuffer-match-face-1)))
   ;; `(ivy-minibuffer-match-face-3 ((t :inherit ivy-minibuffer-match-face-1)))
   ;; `(ivy-minibuffer-match-face-4 ((t :inherit ivy-minibuffer-match-face-1)))
   ;; `(ivy-minibuffer-match-highlight ((t :inherit ivy-minibuffer-match-face-1)))

   `(isearch ((t (:inherit match))))
   `(lazy-highlight ((t (:inherit match))))

   `(dired-directory ((t (:weight bold))))
   `(dired-header ((t (:inherit header-line))))

   ;; `(ag-hit-face ((t :underline t)))
   ))

(provide-theme 'archrome)
