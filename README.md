# Summary

Search for jira issues from Emacs. Quickly insert issues into text files.

## Usage

* jira-show-filter-result
* jira-insert-filter-result-here
* jira-yank-issue

## Configuration

``jira.el`` uses ``url`` package to communicate via rest api and reads password
from ``.authinfo.gpg``. There are few configuration parameter which you can
tweak, see ``jira.el`` so details.

## Philosophy

Existing approach to integrate jira and org-mode is bi-directional
synchronization by [org-jira](https://github.com/baohaojun/org-jira). This is
cool but I prefer different workflow. My idea is:
* quickly search for issues from Emacs
* track only small subset of issues inside text files.

Tracking issues inside text files gives you ability to:
* add private notes to each tasks
* add mini-tasks to each issue *(push person, check result, etc.)*
* use calendar: add deadlines and schedule mini-tasks
