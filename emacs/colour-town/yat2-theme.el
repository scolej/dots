(fringe-mode nil)
(window-divider-mode -1)
(setq-default cursor-in-non-selected-windows nil)

(deftheme yat)

(let ((divider "#e2dcd9"))
  (custom-theme-set-faces
   'yat

   `(default ((t (:foreground "#000000"))))

   `(mode-line ((t (:inherit default :foreground "#999391" :background ,divider :height 100))))
   `(mode-line-inactive ((t (:inherit mode-line))))

   ;; `(fringe ((t (:inherit default :background ,divider))))
   `(fringe ((t (:inherit default :foreground "#eeeeee"))))
   `(vertical-border ((t (:foreground ,divider))))
   ))
