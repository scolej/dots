;;
;; Minimal configuration.
;;

(setq custom-file "~/.emacs.d/ignore-me.el")

(require 'savehist)

(blink-cursor-mode -1)

;; when you inevitably forget: the reason to not use the bar is that
;; clicking to position point is ever so slightly off; but off enought to
;; be disconcerting.
(setq-default
 cursor-type 'box
 cursor-in-non-selected-windows 'hollow)

(fringe-mode nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(recentf-mode -1)

(menu-bar-mode -1)
(setf (alist-get 'menu-bar-lines default-frame-alist) 0)

(savehist-mode 1)
(setq savehist-autosave-interval 120
      history-length 500)

(transient-mark-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(global-so-long-mode 1)

(setq
 load-prefer-newer t
 save-interprogram-paste-before-kill t
 backup-by-copying nil

 backup-directory-alist '((".*" . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 6
 version-control t
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t))
 auto-save-timeout 30
 auto-save-interval 0
 create-lockfiles nil

 inhibit-startup-screen t
 initial-scratch-message nil

 dired-auto-revert-buffer t
 dired-clean-confirm-killing-deleted-buffers nil
 dired-use-ls-dired nil
 dired-recursive-deletes 'top           ; always?

 require-final-newline t
 mode-require-final-newline t

 sentence-end-double-space nil
 set-mark-command-repeat-pop nil
 ring-bell-function 'ignore
 linum-format "%4d"
 mouse-autoselect-window 0.1
 revert-without-query '(".*")
 mouse-wheel-progressive-speed nil
 vc-handled-backends '(git)
 case-fold-search t
 mouse-yank-at-point nil

 bidi-paragraph-direction 'left-to-right
 bidi-inhibit-bpa t

 read-buffer-completion-ignore-case t
 completion-ignore-case t

 ;; Things which this affects:
 ;; - 'up' in Info mode
 scroll-conservatively 1
 scroll-margin 0
 scroll-step 0

 mouse-wheel-scroll-amount '(1 ((shift) . 10) ((control) . 20))
 use-dialog-box nil
 async-shell-command-buffer 'new-buffer

 split-width-threshold 200
 split-height-threshold nil)

(setq-default
 indent-tabs-mode nil
 tab-width 4
 truncate-lines t
 auto-hscroll-mode t
 hscroll-step 10
 hscroll-margin 2
 buffer-file-coding-system 'utf-8-unix)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-z") nil) ; by default this is suspend-frame, i never want this
