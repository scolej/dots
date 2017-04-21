;; Faces to investigate:
;; - no matching paren
;; - magit colours
;; - highlight faces?

(deftheme acme "A simple theme inspired by the Acme editor.")
(custom-make-theme-feature 'acme)

;; Colours
(let ((bg0 "#FDF6DF")
      (bg-dark "#e6b87d")
      (fg0 "#333333")
      (fg-dark "#000000")
      (fg-light "#999999")
      (light-blue "#d5f2fc")
      (light-light-blue "#e7f8fd")
      (dark-blue "#7db8ce")
      (dark-dark-blue "#086989"))
  ;; Attribute groups
  (let ((default-attrs `(:foreground ,fg0 :background ,bg0))
        (emph-attrs `(:foreground ,fg0 :background ,bg0 :weight bold))
        (diminish-attrs `(:foreground ,fg-light :background ,bg0)))

    (custom-theme-set-faces
     'acme

     `(default ((t . (:foreground ,fg0 :background ,bg0))))
     `(minibuffer-prompt ((t . ,emph-attrs)))
     `(cursor ((t . (:foreground ,fg0 :background "#ff0000" :inverse-video nil))))

     `(mode-line-inactive ((t . (:foreground ,fg0 :background ,light-blue))))
     `(mode-line ((t . (:foreground ,fg0 :background ,dark-blue))))
     `(fringe ((t (:foreground ,bg-dark :background ,bg0))))
     `(vertical-border ((t (:foreground ,bg-dark :background ,bg0))))

     `(region ((t (:foreground ,fg0 :background ,light-blue))))

     `(show-paren-match ((t ,emph-attrs)))

     `(isearch ((t (:foreground ,fg0 :background ,light-blue))))
     `(highlight ((t (:foreground ,fg0 :background ,light-blue))))
     `(lazy-highlight ((t (:foreground ,fg0 :background ,light-blue))))
     `(match ((t (:foreground ,fg0 :background ,light-blue))))

     `(font-lock-builtin-face ((t . ,default-attrs)))
     `(font-lock-comment-delimiter-face ((t . ,diminish-attrs)))
     `(font-lock-comment-face ((t . ,diminish-attrs)))
     `(font-lock-constant-face ((t . ,default-attrs)))
     `(font-lock-doc-face ((t . ,diminish-attrs)))
     `(font-lock-function-name-face ((t . (:foreground ,fg-dark :background ,bg0))))
     `(font-lock-keyword-face ((t . (:foreground ,fg-dark :background ,bg0 :weight bold))))
     `(font-lock-negation-char-face ((t . ,default-attrs)))
     `(font-lock-preprocessor-face ((t . ,default-attrs)))
     `(font-lock-regexp-grouping-construct ((t . ,default-attrs)))
     `(font-lock-regexp-grouping-backslash ((t . ,default-attrs)))
     `(font-lock-string-face ((t . ,default-attrs)))
     `(font-lock-type-face ((t . ,default-attrs)))
     `(font-lock-variable-name-face ((t . ,default-attrs)))
     `(font-lock-warning-face ((t . ,default-attrs)))

     )))

(provide-theme 'acme)
