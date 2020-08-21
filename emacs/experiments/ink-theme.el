(deftheme ink)
(let ((fg "#000000")
      (bg "#fff8eb")
      (deep-purple "#7a1670")
      (deep-blue "#010573")
      (bright-blue "#4327cf")
      (emerald "#0e5e29"))
  (custom-theme-set-faces
   'ink

   `(cursor ((t :foreground ,fg :background "#ff0000")))
   `(default ((t :foreground ,fg :background ,bg)))
   `(minibuffer-prompt ((t :foreground ,deep-purple)))

   `(mode-line ((t :background nil , :foreground ,bg :background ,deep-purple)))
   `(mode-line-inactive ((t :background nil , :foreground ,bg :background ,deep-purple)))
   ;; `(mode-line ((t :foreground ,deep-purple :background nil :box (:line-width 2 :style released-button))))
   ;; `(mode-line-inactive ((t :foreground ,deep-purple :background nil :box (:line-width 2 :style released-button))))

   `(org-table ((t :foreground ,bright-blue)))
   `(link ((t :foreground ,deep-blue)))
   `(highlight ((t :inherit link :background "#a3a5d1")))

   `(font-lock-comment-face ((t :foreground ,emerald)))
   `(font-lock-function-name-face ((t :foreground ,deep-blue)))
   `(font-lock-keyword-face ((t :foreground ,deep-purple)))
   `(font-lock-type-face ((t :foreground ,emerald)))

   `(dired-directory ((t :foreground ,deep-purple)))
   `(dired-header ((t :foreground ,emerald)))
   `(vertical-border ((t :background ,deep-purple)))))
(provide-theme 'ink)
