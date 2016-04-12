;; TODO Figure out how to make cua-rectangle-mark-mode not use org-table backspace function
;; TODO Figure out what's going on with isearch mode variables, why when I set isearch-string, the highlighting doesn't match the searching.
;; TODO If char before point is whitespace, C-s should do hungry-delete-backward
;; TODO highlight current line - set fg colour so you can always see it
;; TODO select whole lines like Vim shift-v

;; TODO this --> http://stackoverflow.com/questions/21761971/prevent-emacs-commands-from-showing-new-buffers-in-other-windows
;; use (window-list) and (frame-list)

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

;; Visual set up
(setq linum-format "%4d")
(setq-default mode-line-format (list "%Z %6l %2c >>> %m; %b; %f; %P"))
(setq show-help-function nil)
(show-paren-mode)
(global-hl-line-mode -1)
(set-default 'truncate-lines t)
 ;; Get rid of gross 3d styling
(set-face-attribute 'mode-line-inactive nil :box t)
(set-face-attribute 'mode-line nil :box t)

(defun load-font (f)
  (if (find-font (font-spec :name f))
      (progn (set-default-font f)
             (add-to-list 'default-frame-alist `(font . ,f)))
    nil))

;; Don't forget, for OSX:
;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 999
;; to turn of anti-aliasing.

(when (display-graphic-p)
  (or
   (load-font "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-2")
   (load-font "Menlo 11")
   (load-font "Consolas 12")
   (load-font "Courier New:pixelsize=15:antialias=none")))

(or
 (ignore-errors (load-theme 'solarized-light) t)
 (ignore-errors (load-theme 'white-sand) t))

(set-face-attribute 'cursor nil :background "#ff0000")

;; No tabs!!
(setq-default indent-tabs-mode nil)

;; (ffap-bindings)
(ido-mode)
;; (setq mouse-autoselect-window nil)
(windmove-default-keybindings)

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(defun switch-to-buffer-menu ()
  (interactive)
  (let ((b (get-buffer "*Buffer List*")))
    (if b (progn
            (switch-to-buffer b)
            (revert-buffer))
      (buffer-menu))))

(global-set-key (kbd "C-`") 'switch-to-buffer-menu)

(defun perm ()
  (interactive)
  (set-window-dedicated-p (get-buffer-window) t))
  ;;(setq window-size-fixed t))

(global-set-key (kbd "<C-S-left>") (lambda () (interactive) (forward-whitespace -1)))
(global-set-key (kbd "<C-S-right>") (lambda () (interactive) (forward-whitespace 1)))

;; ;; If there is a block of whitespace before point, then C-backspace should delete only the whitespace and nothing more.
;; (global-set-key (kbd "<C-backspace>")
;;                 (lambda ()
;;                   (interactive)
;;                   (if (or (equal (char-before) ?\s)
;;                           (equal (char-before) ?\n)
;;                           (equal (char-before) ?\r))
;;                       (hungry-delete-backward 1)
;;                     (backward-kill-word 1))))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
;; (global-set-key (kbd "<M-SPC>") 'ido-switch-buffer)
;; (global-set-key (kbd "M-`") 'ido-switch-buffer)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x f") 'helm-find-files)

(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; (global-set-key (kbd "M-s") 'exchange-point-and-mark)

;; Multi cursor bindings
(when (require 'multiple-cursors nil :noerror)
  (global-set-key (kbd "C-c e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c p") 'mc/unmark-next-like-this))

(when (require 'expand-region nil :noerror)
  (global-set-key (kbd "M-u") 'er/expand-region))

;; Try and get the escape key doing more C-g like stuff.
;; (define-key isearch-mode-map [escape] 'isearch-abort)
;; (define-key isearch-mode-map "\e" 'isearch-abort)
;; (define-key Buffer-menu-mode-map [escape] 'quit-window)

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
  ;; (find-file (ffap-guesser))
  (if (not (equal line-num 0))
      (goto-line line-num)))
(global-set-key (kbd "C-c o") 'find-file-at-point-with-line)

(global-set-key (kbd "C-z") 'undo)

(when (require 'dired-details+ nil :noerror)
  (setf dired-details-propagate-flag t)
  (setf dired-details-hidden-string "")
  (add-hook 'dired-mode (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)))

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-c d") 'duplicate-line)

;; (global-set-key (kbd "C-c l")
(global-set-key (kbd "<M-SPC>")
                (lambda ()
                  (interactive)
                  (move-beginning-of-line 1)
                  (set-mark (point))
                  (move-beginning-of-line 2)))

;; If there is no active selection, I want the copy and cut commands to operate on the whole line

(global-set-key (kbd "M-w")
                (lambda ()
                  (interactive)
                  (if (use-region-p)
                      (kill-ring-save (mark) (point))
                    (kill-ring-save (point-at-bol) (point-at-eol)))))

(global-set-key (kbd "C-w")
                (lambda ()
                  (interactive)
                  (if (use-region-p)
                      (kill-region (mark) (point))
                    (kill-whole-line))))

;; (setq cua-rectangle-mark-mode-hook nil)
;; (add-hook 'cua-rectangle-mark-mode-hook
;;           (lambda ()
;;             (set-key

(when (require 'drag-stuff nil :noerror)
  (drag-stuff-global-mode))

(global-set-key (kbd "<C-return>") 'cua-rectangle-mark-mode)
(global-set-key (kbd "C-v") 'yank)

(global-set-key (kbd "C-c -") 'text-scale-decrease)
(global-set-key (kbd "C-c +") 'text-scale-increase)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(delete-selection-mode 1)

;; Stop polluting the entire filesystem with backup files
(if (boundp '*my-backup-dir*)
    (let ((dir *my-backup-dir*))
      (setq backup-directory-alist
            `((".*" . , dir)))
      (setq auto-save-file-name-transforms
            `((".*" , dir t))))
  (message "Don't forget to specify a backup directory!"))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(set-default 'truncate-lines t)

(defun ttl ()
  (interactive)
  (toggle-truncate-lines))

(defun vlm ()
  (interactive)
  (visual-line-mode))

(defun arm ()
  (interactive)
  (auto-revert-mode))

(defun rev ()
  (interactive)
  (revert-buffer t t))

;; TODO this is awful.
(setf isearch-mode-hook nil)
(add-hook 'isearch-mode-hook
          (lambda ()
            (if (use-region-p)
                (let ((str (buffer-substring-no-properties (point) (mark))))
                           (progn (setq isearch-string str)
                                  (setq isearch-message str)
                                  (deactivate-mark))))))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "<C-tab>") 'switch-to-previous-buffer)

;; ----------

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


;; https://www.masteringemacs.org/article/searching-buffers-occur-mode

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; ----------

(defun generate-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

;; (require 'shackle)
;; (setq shackle-default-rule '(:same t))

(setq split-height-threshold 1200)
(setq split-width-threshold 2000)

;; Stop using bold fonts
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(setq-default truncate-partial-width-windows 0)

(setq buffer-menu-sort-column 4)

(setq speedbar-directory-unshown-regexp "^$")

(server-start)
