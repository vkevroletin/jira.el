(require 'f)

(defvar jira-support-path
  (f-dirname load-file-name))

(defvar jira-features-path
  (f-parent jira-support-path))

(defvar jira-root-path
  (f-parent jira-features-path))

(add-to-list 'load-path jira-root-path)

(require 'jira)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
