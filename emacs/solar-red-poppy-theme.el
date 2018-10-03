;; Looks like you can't use this with the native theme loading functionality.
;; https://github.com/bbatsov/solarized-emacs/issues/196

(require 'solarized)

(deftheme solar-red-poppy "Solarized with some customizations.")

(create-solarized-theme
 'light
 'solar-red-poppy
 (lambda ()
   (custom-theme-set-faces
    'solar-red-poppy
    '(cursor ((t (:background "#ff0000"))))
    ;; Boxy
    ;; '(mode-line ((t (:box (:style released-button) :underline nil))))
    ;; '(mode-line-inactive ((t (:box (:style released-button) :underline nil))))
    ;; Not boxy
    `(mode-line ((t (:background ,base02 :foreground ,base1 :box nil :underline nil))))
    `(mode-line-inactive ((t (:inherit mode-line))))
    )))

