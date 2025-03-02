(defun fuzzy-score (query test-string)
  (setf test-string (downcase test-string))
  (let ((score 0))
    (dolist (c (string-to-list query) score)
      (let ((pos (seq-position test-string c)))
        (cond
         ((null pos)
          (setf score (- score 1))
          (message "deduct"))
         ((= pos 0)
          (setf score (+ 10 score)
                test-string (substring test-string 1))
          (message "bonus"))
         ((> pos 0)
          (setf score (+ 1 score)
                test-string (substring test-string (1+ pos)))
          (message "norm")))))))

(fuzzy-score "abc" "alpha beta gamma")
(fuzzy-score "abg" "alpha beta gamma")

(fuzzy-score "op/too/stop/mars" "operations/tools/stops/stops/src/main.rs")
(fuzzy-score "op/too/stop/mars" "operations/terraform/accounts/excelexporter/corp-ami-sharing/terraform-validated-accounts-excelexporter-corp-ami-sharing.products.yml")

(fuzzy-score "registrreadm" "ci-bots/docker-custodian/registrar/docker-custodian-registrar.products.yml")
(fuzzy-score "registrreadm" "")

