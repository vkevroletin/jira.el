;;; -*- lexical-binding: t -*-

(when (require 'undercover nil t)
  (undercover "jiraffe.el"))

(require 'ert)
(require 'jiraffe)
