(set-face-attribute
 'mode-line nil
 :height 75
 :box nil
 ;; :overline nil :underline nil
 ;; :background "#f3f3f3"
 ;; :foreground "#000"
 ;; :inverse-video nil
 )

(set-face-attribute
 'mode-line-inactive nil
 :inherit 'mode-line
 :foreground 'unspecified :background 'unspecified
 :box 'unspecified :weight 'unspecified)

(set-face-attribute
 'fringe nil
 :inherit 'default
 :background 'unspecified
 :foreground 'unspecified)

(set-face-attribute
 'vertical-border nil
 :inherit 'mode-line :inverse-video t)

(set-face-attribute
 'fixed-pitch-serif nil :family 'unspecified)
