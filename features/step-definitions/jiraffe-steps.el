;;; -*- lexical-binding: t; -*-
;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I am in empty buffer$"
  (lambda ()
    (with-current-buffer (get-buffer-create (generate-new-buffer-name "text"))
      (swith-to-buffer (current-buffer)))))

(Given "^base-url points to \"\\(.+\\)\"$"
  (lambda (url)
    (setq jiraffe-base-url url)))

(And "^\"\\(.+\\)\" added to quick-filters as \"\\(.+\\)\"$"
  (lambda (jql name)
    (add-to-list 'jiraffe-quick-filters
                 (cons name jql))))

(And "authentication is disabled$"
  (lambda ()
    (setq jiraffe-authenticate '())))

(Then "^Eventually I should see \"\\(.+\\)\" in buffer$"
  (lambda (issue-key done-token)
    (run-with-timer 0
                    0.1
                    (lambda ()
                      (save-excursion
                        (save-match-data
                          (goto-char (point-min))
                          (when (search-forward issue-key nil t)
                            (funcall done-token))))))))
