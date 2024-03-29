(defun rust-root ()
  (or (locate-dominating-file default-directory "Cargo.toml")
      (error "not in a rust project")))

(defun rust-open-doc ()
  (interactive)
  (let ((default-directory (rust-root)))
    (start-process-shell-command "cargo" "*rust doc*"
                                 "cargo doc --open")))

(defun rust-open-doc-book ()
  (interactive)
  (let ((default-directory (rust-root)))
    (start-process-shell-command "rustup" "*rust doc*"
                                 "rustup doc --book")))

(defun rust-open-doc-core ()
  (interactive)
  (let ((default-directory (rust-root)))
    (start-process-shell-command "rustup" "*rust doc*"
                                 "rustup doc --core")))

(setq rust-format-on-save nil)

(defun rust-customizations ()
  (setq-local inhibit-clean-trailing-whitespace-mode t))

(add-hook 'rust-mode-hook 'rust-customizations)
(add-hook 'rust-mode-hook 'corfu-mode)

;; (add-to-list 'project-vc-ignores "target/*")

(setq lsp-rust-analyzer-cargo-watch-command "clippy")

(setq lsp-rust-analyzer-cargo-override-command
      nil
      ;; (vector "rust-analyzer"
      ;;         ;;"--target-dir" "tmp-ra"
      ;;         ;;"--message-format=json"
      ;;         "diagnostics"
      ;;         )
      )
