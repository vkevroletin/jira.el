;;; -*- lexical-binding: t -*-

(when (require 'undercover nil t)
  (undercover "jiraffe.el" (:report-file "/tmp/local-report.json") (:send-report nil)))

(require 'ert)
(require 'jiraffe)
