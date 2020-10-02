(add-to-list 'default-frame-alist '(cursor-color . "#ff0000"))

(set-face-attribute 'mode-line nil
                    :height 75 :box nil :overline nil :underline nil
                    :foreground "#555555" :background "#eeeeee"
                    :inverse-video nil)
(set-face-attribute 'mode-line-inactive nil
                    :inherit 'mode-line
                    :foreground nil :background nil
                    :box nil :weight 'normal)

(set-face-attribute 'fringe nil
                    :inherit 'default
                    :background "#ffffff"
                    :foreground nil)
(set-face-attribute 'vertical-border nil
                    :inherit 'mode-line :inverse-video t)

(load-theme 'modus-operandi)

;; (set-face-attribute 'org-block-begin-line nil :background nil :foreground "gray")
