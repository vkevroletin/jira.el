;; -*- lexical-binding: t -*-
;;; jira.el --- A library to retrieve tasks from jira into text files
;;
;; Public domain.

;; Author: Vasiliy Kevroletin <kevroletin@gmail.com>
;; Maintainer: Vasiliy Kevroletin <kevroletin@gmail.com>
;; Keywords: jira
;; Package-Version: 0

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is unfinished work. Probably it will evolve into something useful.

;;; Code:
;;

(require 'request)
(require 'json)
(require 'f)
(require 'dash)
(require 'lifted)
(require 'restclient)

(defcustom jira-base-url "https://jira.com"
  "Url like https://jira.com"
  :group 'jira
  :type 'string)

(defcustom jira-authenticate t
  "Whether to send authentication info from .authinfo.gpg to server"
  :group 'jira
  :type 'boolean)

(defcustom jira-my-issues-jql
  "assignee=currentUser() and resolution=unresolved and \"Landing Zone\" is not empty"
  "JQL used by *my-issues* functions"
  :group 'jira
  :type 'string)

(defcustom jira-quick-filters
  `(("My lz issues" . ,jira-my-issues-jql)
    ("My issues" . "assignee=currentUser() and resolution=unresolved"))
  "Quick means they are saved within emacs and require no fetch tile"
  :group 'jira
  :type 'alist)

(defcustom jira-pending-request-placeholder "{jira-pending-request}"
  "This string is inserted into buffer till end of asynchronous
retrieving of data."
  :group 'jira
  :type 'string)

(defcustom jira--debug-save-response-to-file '()
  "Path to file"
  :group 'jira
  :type 'string)

(defcustom jira--debug-read-response-from-file '()
  "Path to file"
  :group 'jira
  :type 'string)

(defun jira--domain ()
  (-first-item (s-split "/" (-last-item (s-split "://" jira-base-url)))))

(defun jira--read-secret ()
  (let* ((auth (nth 0 (auth-source-search :host (jira--domain)
                                          :requires '(user secret))))
         (pass (funcall (plist-get auth :secret)))
         (user (plist-get auth :user)))
    (base64-encode-string (concat user ":" pass))))

(defun jira--at-helper (key data)
  (if (numberp key)
      (if (<= (length data) key)
          '()
        (elt data key))
    (cdr (assoc key data))))

(defun jira--at (keys data)
  (if (not (consp keys))
      (jira--at-helper keys data)
    (-let (((x . xs) keys))
      (if xs
          (jira--at xs (jira--at-helper x data))
        (jira--at-helper x data)))))

(defun jira--filter-nils (&rest data)
  (-filter #'identity data))

(defun jira--truncate-url-path (x)
  (-if-let (((from . to)) (s-matched-positions-all "[^:\/]\/" x))
      (s-left (+ 1 from) x)
    x))

(defun jira--rest-url (x)
  (format "%s/rest/api/latest/%s" (jira--truncate-url-path jira-base-url) x))

(defun jira--issue-browse-url (key)
  (format "%s/browse/%s" jira-base-url key))

(defun jira--headers ()
  (jira--filter-nils
   (when jira-authenticate
     `("Authntication" . ,(concat "Basic " (jira--read-secret))))
   '("Content-type" . "application/json")))

(defun jira--encode-get-params (params)
  "Expecting params to be alist"
  (s-join "&" (--map (format "%s=%s" (car it) (cdr it)) params)))

(defun jira--rest-url-with-get-params (mini-url &optional params)
  (if params
      (let ((sep (if (equal "/" (s-right 1 mini-url)) "?" "/?")))
          (concat (jira--rest-url mini-url) sep (jira--encode-get-params params)))
    (jira--rest-url mini-url)))

(defun jira--parse-http-response-to-json (buffer)
  "Maps buffer -> json"
  ;; This is tricky moment: url-retreive doesn't detect utf-8 response
  ;; automatically (emacs shows characters as \342\240... So we "reuse"
  ;; restclient-decode-response to get utf-8 buffer
  (with-current-buffer
      (restclient-decode-response buffer (get-buffer-create "*jira-data*") t)
    (progn
      (goto-char (point-min))
      ;; Skip headers
      (re-search-forward "^$")
      ;; Parse rest of buffer as json
      (let* ((json-object-type 'alist)
             (json-array-type 'vector))
        (json-read)))))

(defun jira--maybe-dump-responce (result)
  (when jira--debug-save-response-to-file
    (f-write-text (json-encode result) 'utf-8 jira--debug-save-response-to-file)))

(defun jira--add-parsing-to-callback (callback)
  (lambda (status)
    (-when-let (err (plist-get status :error))
      (goto-char (point-min))
      (let ((first-line (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position))))
        (signal (car err) (list first-line))))
    (let ((res (jira--parse-http-response-to-json (current-buffer))))
      (jira--maybe-dump-responce res)
      (funcall callback res))))

(defun jira--retrieve-common-debug (method mini-url callback &optional params)
  method   ;; hide unused parameter warning
  mini-url
  callback
  params
  (funcall callback
           (json-read-from-string (f-read-text jira--debug-read-response-from-file))))

(defun jira--retrieve-common-normal (method mini-url callback &optional params)
  (let ((url-request-method method)
        (url-request-extra-headers (jira--headers))
        (url-request-data '())
        (full-url '()))
    (when (and (equal method "POST") params)
      (setq url-request-data (json-encode params)))
    (if (equal method "GET")
        (setq full-url (jira--rest-url-with-get-params mini-url params))
      (setq full-url (jira--rest-url mini-url)))

    (url-retrieve full-url (jira--add-parsing-to-callback callback))))

(defun jira--retrieve-common (method mini-url callback &optional params)
  (if jira--debug-read-response-from-file
      (jira--retrieve-common-debug method mini-url callback params)
    (jira--retrieve-common-normal method mini-url callback params)))

(defun jira-get (mini-url callback &optional params)
  "Retrieves data from jira asynchronously using GET request.
Calls callback only in case of success with json parsed into
elisp alists and vectors. Gives no guarantees about about saving
excursion and current buffer."
  (jira--retrieve-common "GET" mini-url callback params))

(defun jira-get-signal (mini-url &optional params)
  (lifted:signal
   (lambda (subscriber)
     (jira-get mini-url
               (lambda (x) (funcall subscriber :send-next x))
               params))))

(defun jira-post (mini-url callback &optional body-params)
  "Retrieves data from jira asynchronously using POST request.
Calls callback only in case of success with json parsed into
elisp alists and vectors. body-params are encoded into json.
Gives no guarantees about about saving excursion and current
buffer."
  (jira--retrieve-common "POST" mini-url callback body-params))

(defun jira-post-signal (mini-url &optional params)
  (lifted:signal
   (lambda (subscriber)
     (jira-post mini-url
                (lambda (x) (funcall subscriber :send-next x))
                params))))

(defun jira-jql-filter-signal (jql)
  "Sequence of mini-issues"
  (lifted:map
   #'jira--minify-jira-list
   (jira-post-signal "search" `(("jql" . ,jql)))))

(defun jira--minify-jira-list (xs)
  (-map #'jira--minify-jira (jira--at 'issues xs)))

(defun jira--minify-jira (x)
  "Let call result 'issue' which is simplified representation of jira"
  (list
   (assoc 'key x)
   (cons 'labels      (jira--at '(fields labels) x))
   (cons 'project     (jira--at '(fields project name) x))
   (cons 'project_key (jira--at '(fields project key) x))
   (cons 'issue_type  (jira--at '(fields issuetype name) x))
   (cons 'summary     (jira--at '(fields summary) x))))

(defun jira--issue-caption (issue)
  (format "[[%s][%s]]"
          (jira--issue-browse-url (jira--at 'key issue))
          (s-replace-all '(("[" . "{") ("]" . "}")) (jira--at 'summary issue))))

(defun jira--issue-to-org-task (x)
  (s-join
   "\n"
   (list
    (concat "* MAYBE " (jira--issue-caption x)))))

(defun jira--find-issue-in-buffer (issue buffer)
  (let ((pattern (format "*+ \\w+ %s" (regexp-quote (jira--issue-caption issue)))))
    (save-excursion
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward pattern '() t) ;; suppress error
          t)))))

(defun jira--my-issues-signal ()
  (jira-jql-filter-signal jira-my-issues-jql))

(defun jira--kill-line ()
  "Kill line without kill ring"
  (let ((beg (point)))
    (forward-line 1)
    (delete-region beg (point))))

(defun jira--insert-jiras (filter-signal)
  "This function could be described using pseudo code: signal
provides issues => insert each issue into current pos. Tricky
moment is asynchronous nature of data retrieval. Instead of
blocking we remember where to place result. We mark this place in
buffer by magic string. Later magic string is replaced by result.
User can remove magic string to cancel operation."
  (goto-char (line-beginning-position))
  (insert (format "%s\n\n" jira-pending-request-placeholder))
  (forward-line -2)
  (let ((buffer (current-buffer))
        (placeholder-position (point)))
    (funcall filter-signal
             :subscribe-next
             (lambda (xs)
               (with-current-buffer buffer
                 (save-excursion
                   (goto-char placeholder-position)
                   (when (looking-at (regexp-quote jira-pending-request-placeholder))
                     (jira--kill-line)
                     (--each xs
                       (unless (jira--find-issue-in-buffer it (current-buffer))
                         (insert (jira--issue-to-org-task it))
                         (insert "\n\n")))
                     (jira--kill-line))
                   (message "Done")))
               (goto-char placeholder-position)))))

(defun jira--filters-helm-sources ()
  (jira--filter-nils
   (when jira-quick-filters
     (helm-build-sync-source "Quick filters"
       :candidates (--map (format "%s | %s" (car it) (cdr it)) jira-quick-filters)
       :fuzzy-match t))))

(defun jira-insert-filter-result-here ()
  (interactive)
  (-if-let (choice (helm :sources (jira--filters-helm-sources) :buffer "*jira-filters*"))
      (let ((jql (-last-item (s-split " | " choice))))
        (jira--insert-jiras (jira-jql-filter-signal jql)))))

(defun jira-insert-my-issues-here ()
  (interactive)
  (jira--insert-jiras (jira--my-issues-signal)))

(provide 'jira)

;;; jira.el ends here
