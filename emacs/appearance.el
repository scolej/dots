(defun theme-tweaks ()
  (set-face-attribute
   'mode-line nil
   :height 0.7
   :box nil)
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
  ;; (set-face-attribute 'fixed-pitch-serif nil :family 'unspecified)
  )

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
