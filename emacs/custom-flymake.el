(setq flymake-proc-allowed-file-name-masks
      (cons '(".+\\.c$"
              flymake-proc-simple-make-init
              flymake-proc-simple-cleanup
              flymake-proc-get-real-file-name)
            flymake-proc-allowed-file-name-masks))
