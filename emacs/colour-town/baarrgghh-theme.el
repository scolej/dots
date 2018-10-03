(deftheme arrgghh "A chill green theme.")

(let ((i-dont-care-grey "#888888")
      (normal-fg "#3dc68d")
      (dark-fg "#134430")
      (background "#0f3022")
      (dark-background "#0b2319"))
  (custom-theme-set-faces
   'arrgghh

   `(cursor ((t (:background "#ff0000"))))
   `(region ((t (:background "#2a8c63"))))
   `(default ((t (:foreground ,normal-fg :background ,background))))
   `(hl-line ((t (:background "#134c45"))))

   `(mode-line ((t (:box nil :foreground ,dark-fg :background ,dark-background))))
   `(mode-line-inactive ((t (:inherit mode-line))))

   `(trailing-whitespace ((t (:background "#ffdddd"))))

   `(show-paren-match ((t (:foreground "#ff0000"))))

   `(line-number ((t (:foreground ,i-dont-care-grey :background "#ff0000"))))
   `(line-number-current-line ((t (:inherit line-number))))

   `(link ((t (:underline t))))
   `(link-visited ((t (:inherit link))))

   `(font-lock-constant-face ((t (:inherit default))))
   `(font-lock-variable-name-face ((t (:inherit default))))
   `(font-lock-function-name-face ((t (:inherit default))))
   `(font-lock-builtin-face ((t (:inherit default))))
   `(font-lock-keyword-face ((t (:inherit default :weight bold))))
   `(font-lock-type-face ((t (:inherit default))))
   `(font-lock-string-face ((t (:foreground "#2e9b6e"))))
   `(font-lock-comment-face ((t (:foreground "#48756f"))))

   `(fringe ((t (:inherit default :foreground ,i-dont-care-grey))))
   `(vertical-border ((t :foreground ,dark-background)))

   `(header-line ((t :inherit default)))
   `(minibuffer-prompt ((t :inherit default)))

   `(ivy-current-match ((t :inherit region)))
   `(ivy-minibuffer-match-face-1 ((t :inherit hl-line)))
   `(ivy-minibuffer-match-face-2 ((t :inherit ivy-minibuffer-match-face-1)))
   `(ivy-minibuffer-match-face-3 ((t :inherit ivy-minibuffer-match-face-1)))
   `(ivy-minibuffer-match-face-4 ((t :inherit ivy-minibuffer-match-face-1)))
   `(ivy-minibuffer-match-highlight ((t :inherit ivy-minibuffer-match-face-1)))

   `(isearch ((t :inherit match)))
   `(lazy-highlight ((t :inherit match)))
   ))

(provide-theme 'arrgghh)
