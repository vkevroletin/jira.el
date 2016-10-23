# Summary

Fetch jira issues into files.

Existing approach to integrate jira and org-mode is bi-directional
synchronization by [org-jira](https://github.com/baohaojun/org-jira). This is
cool but I prefer different workflow. My idea is to manage read-only issues
inside my org files to be able to:

* add private notes to each tasks
* add mini-tasks to each issue *(push someone, check result, etc.)*
* use calendar: add deadlines and schedule mini-tasks
* use different filters to fetch mine tasks and tasks of my teammates

## Usage

``jira.el`` uses ``url`` package to communicate via rest api and stores password
into ``.authinfo.gpg``. There is few configuration parameter which you can
tweak, see ``jira.el`` so details.

Currently there is only one useful interactive function: **jira-insert-my-issues-here**.

## TODO

* Improve interactive ``insert-issues`` functions:
  * honor current nesting level;
  * search to defines jiras across several existing files;
  * helm interface to select favorite filters.
* Quick temporary buffer for search result with helm interface to filters/last
  searches.
* Improve representation of each issue:
  * add labels;
  * add assignee with link to filter which shows his/her landing zone.
* Implement updating process:
  * show changed for each task and provide interface to update it manually.
* Non interactive function to synchronize issues in files.
