;;; -*- lexical-binding: t -*-

(when (require 'undercover nil t)
  (undercover "jira.el" (:report-file "/tmp/local-report.json") (:send-report nil)))

(require 'ert)
(require 'jira)
