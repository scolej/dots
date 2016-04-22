;; TODO dired file heat-map highlighting
;; TODO C-l to cycle through line number modes

(when (require 'package nil :noerror)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

(require 'org-table)

;; Disable annoying things
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)))
(setq mouse-wheel-progressive-speed nil)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq linum-format "%4d")
(setq-default mode-line-format (list "%Z %6l %2c >>> %m; %b; %f; %P"))
(setq show-help-function nil)
(show-paren-mode)
(setq show-paren-style 'expression)
;; (global-hl-line-mode -1)
(set-default 'truncate-lines t)

 ;; Get rid of disgusting 3D styling
(set-face-attribute 'mode-line-inactive nil :box t)
(set-face-attribute 'mode-line nil :box t)

(defun load-font (f)
  (if (find-font (font-spec :name f))
      (progn (set-default-font f)
             (add-to-list 'default-frame-alist `(font . ,f)))
    nil))

(when (display-graphic-p)
  (or
   (load-font "Mono 9")
   (load-font "Consolas 8")))

(or
 (ignore-errors (load-theme 'solarized-light) t)
 (ignore-errors (load-theme 'solarized-dark) t)
 (ignore-errors (load-theme 'white-sand) t))

(set-face-attribute 'cursor nil :background "#ff0000")

;; No tabs!!
(setq-default indent-tabs-mode nil)

(ido-mode)
;; (setq mouse-autoselect-window nil)
(windmove-default-keybindings)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(defun perm ()
  "Try and get the window to stay where it is and not be replaced or moved about."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) t))

(defun switch-to-buffer-menu ()
  (interactive)
  (let ((b (get-buffer "*Buffer List*")))
    (if b (progn
            (switch-to-buffer b)
            (revert-buffer))
      (buffer-menu))))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-`") 'switch-to-buffer-menu)

;; Multi cursor bindings
;; (when (require 'multiple-cursors nil :noerror)
;;   (global-set-key (kbd "C-c e") 'mc/edit-ends-of-lines)
;;   (global-set-key (kbd "C-c a") 'mc/edit-beginnings-of-lines)
;;   (global-set-key (kbd "C-c m") 'mc/edit-lines)
;;   (global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
;;   (global-set-key (kbd "C-c p") 'mc/unmark-next-like-this))

;; http://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
        (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  (if (not (equal line-num 0))
      (goto-line line-num)))
(global-set-key (kbd "C-c o") 'find-file-at-point-with-line)

(global-set-key (kbd "C-z") 'undo)

(when (require 'dired-details+ nil :noerror)
  (setf dired-details-propagate-flag t)
  (setf dired-details-hidden-string "")
  (add-hook 'dired-mode (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)))

(setf text-scale-mode-step 1.05)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; (delete-selection-mode 1)

;; Stop polluting the entire filesystem with backup files
(if (boundp '*my-backup-dir*)
    (let ((dir *my-backup-dir*))
      (setq backup-directory-alist `((".*" . , dir)))
      (setq auto-save-file-name-transforms `((".*" , dir t)))))

(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Delete trailing whitespace on save

(defun words-dammit ()
  (interactive)
  (toggle-truncate-lines)
  (visual-line-mode))

(defun arm ()
  (interactive)
  (auto-revert-mode))

;; TODO this is awful.
;; Want to automatically search for the selected string if we C-s with a live selection
;; (setf isearch-mode-hook nil)
;; (add-hook 'isearch-mode-hook
;;           (lambda ()
;;             (if (use-region-p)
;;                 (let ((str (buffer-substring-no-properties (point) (mark))))
;;                            (progn (setq isearch-string str)
;;                                   (setq isearch-message str)
;;                                   (deactivate-mark))))))

(global-set-key (kbd "C-c h")
                (lambda ()
                  (interactive)
                  (if (not (= (point) (mark)))
                      (highlight-regexp (buffer-substring-no-properties (point) (mark))))))

(global-set-key (kbd "C-c u")
                (lambda ()
                  (interactive)
                  (if (not (= (point) (mark)))
                      (unhighlight-regexp (buffer-substring-no-properties (point) (mark))))))

(defun gen-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

;; Stop using bold fonts
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(defun hi-log ()
  (interactive)
  (unhighlight-regexp t)
  (highlight-regexp "^INFO" 'hi-blue)
  (highlight-regexp "^WARNING" 'hi-yellow)
  (highlight-regexp "^SEVERE" 'hi-red-b))

;; (setq-default truncate-partial-width-windows 0)
;; (setq buffer-menu-sort-column 4)
;; (setq speedbar-directory-unshown-regexp "^$")

(global-relative-line-numbers-mode)

(evil-mode)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-motion-state-map (kbd "<remap> <evil-start-of-line>") 'evil-start-of-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-start-of-line>") 'evil-start-of-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-end-of-line>") 'evil-end-of-visual-line)
(setq-default evil-cross-lines t)

(server-start)
