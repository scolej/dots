(defun theme-tweaks ()
  (set-face-attribute
   'mode-line nil
   :height 1.0
   :box nil
   :background "#ccccff")
  (set-face-attribute
   'mode-line-inactive nil
   :inherit 'mode-line
   :foreground 'unspecified :background "#eeeeee"
   :box 'unspecified :weight 'unspecified)
  (set-face-attribute
   'fringe nil
   :inherit 'default
   :background "#eeeeee"
   :foreground 'unspecified)
  (set-face-attribute
   'vertical-border nil
   :inherit 'mode-line :inverse-video t)
  ;; (set-face-attribute 'fixed-pitch-serif nil :family 'unspecified)
  (set-face-attribute
   'cursor nil
   :background "#f200ff"))

(defun switch-theme (new-theme)
  (interactive)
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme new-theme))

(defun switch-theme-dark ()
  (interactive)
  (switch-theme 'modus-vivendi)
  (theme-tweaks))

(defun switch-theme-light ()
  (interactive)
  (switch-theme 'modus-operandi)
  (theme-tweaks))

(require 'markdown-mode)
(set-face-attribute
 'markdown-header-face-1 nil
 :height 1.3)
(set-face-attribute
 'markdown-header-face-2 nil
 :height 1.2)
(set-face-attribute
 'markdown-header-face-3 nil
 :height 1.1)

(set-face-attribute
 'tab-bar-tab nil
 :box 'unspecified
 :background "#eeeeff")

(set-face-attribute
 'tab-bar-tab-inactive nil
 :box 'unspecified)



(set-face-attribute
   'fringe nil
   :inherit 'default
   :background 'unspecified
   :foreground "#aaaaaa")
