(require 'org)
(require 'org-table)
(require 'org-tempo)

(setq org-startup-folded nil
      org-adapt-indentation nil)

;; (define-key org-mode-map (kbd "<tab>") nil)
;; (define-key org-mode-map (kbd "<C-tab>") nil)
(define-key orgtbl-mode-map (kbd "<S-return>") nil)
(define-key orgtbl-mode-map (kbd "<return>") nil)
