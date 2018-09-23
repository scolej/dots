(package-refresh-contents)
(package-install 'expand-region)
(package-install 'slime)

(require 'slime)

(mapc #'disable-theme custom-enabled-themes)
