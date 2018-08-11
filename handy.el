(package-refresh-contents)
(package-install 'swank)
(package-install 'slime)

(require 'slime)

(mapc #'disable-theme custom-enabled-themes)
