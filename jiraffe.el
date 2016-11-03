;;; jiraffe.el --- A library to retrieve tasks from jira into text files -*- lexical-binding: t; -*-
;;
;; Public domain.

;; Author: Vasiliy Kevroletin <kevroletin@gmail.com>
;; Maintainer: Vasiliy Kevroletin <kevroletin@gmail.com>
;; Keywords: jira
;; Package-Version: 20161029.1309
;; Package-X-Original-Version: 0

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; Home page https://github.com/vkevroletin/jiraffe.el

;;; Code:
;;

(require 'dash)
(require 'f)
(require 'helm)
(require 'json)
(require 'lifted)
(require 'request)
(require 'restclient)

;;;###autoload
(defcustom jiraffe-base-url "https://jira.com"
  "Url like https://jira.com"
  :group 'jiraffe
  :type 'string)

;;;###autoload
(defcustom jiraffe-authenticate t
  "Whether to send authentication info from .authinfo.gpg to server"
  :group 'jiraffe
  :type 'boolean)

;;;###autoload
(defcustom jiraffe-my-issues-jql
  "assignee=currentUser() and resolution=unresolved and \"Landing Zone\" is not empty"
  "JQL used by *my-issues* functions"
  :group 'jiraffe
  :type 'string)

;;;###autoload
(defcustom jiraffe-quick-filters
  `(("My landing zone issues"    . ,jiraffe-my-issues-jql)
    ("My current issues"         . "assignee=currentUser() and resolution=unresolved")
    ("My issues in progress"     . "assignee=currentUser() and status=\"In progress\"")
    ("My issues + {text search}" . "assignee=currentUser() and text ~ \"{text}\""))
  "Quick means they are saved within emacs and require no fetch tile"
  :group 'jiraffe
  :type 'alist)

;;;###autoload
(defcustom jiraffe-issue-files
  '()
  "List of entities where jira.el searches for existing
issues (in addition to current file) to prevent duplicates. Each
item could be file of directory. If item is directory then it is
same as adding all *.org files within this directory."
  :group 'jiraffe
  :type 'list)

;;;###autoload
(defcustom jiraffe-pending-request-placeholder "{jiraffe-pending-request}"
  "This string is inserted into buffer till end of asynchronous
retrieving of data."
  :group 'jiraffe
  :type 'string)

(defcustom jiraffe--debug-save-response-to-file '()
  "Path to file"
  :group 'jiraffe
  :type 'string)

(defcustom jiraffe--debug-read-response-from-file '()
  "Path to file"
  :group 'jiraffe
  :type 'string)

(defvar jiraffe--templates-history '())

(defun jiraffe--domain ()
  (-first-item (s-split "/" (-last-item (s-split "://" jiraffe-base-url)))))

(defun jiraffe--read-secret ()
  (let* ((auth (nth 0 (auth-source-search :host (jiraffe--domain)
                                          :requires '(user secret))))
         (pass (funcall (plist-get auth :secret)))
         (user (plist-get auth :user)))
    (base64-encode-string (concat user ":" pass))))

(defun jiraffe--at-helper (key data)
  (if (numberp key)
      (if (<= (length data) key)
          '()
        (elt data key))
    (cdr (assoc key data))))

(defun jiraffe--at (keys data)
  (if (not (consp keys))
      (jiraffe--at-helper keys data)
    (-let (((x . xs) keys))
      (if xs
          (jiraffe--at xs (jiraffe--at-helper x data))
        (jiraffe--at-helper x data)))))

(defun jiraffe--filter-nils (&rest data)
  (-filter #'identity data))

(defun jiraffe--truncate-url-path (x)
  (-if-let (((from . to)) (s-matched-positions-all "[^:\/]\/" x))
      (s-left (+ 1 from) x)
    x))

(defun jiraffe--rest-url (x)
  (format "%s/rest/api/latest/%s" (jiraffe--truncate-url-path jiraffe-base-url) x))

(defun jiraffe--issue-browse-url (key)
  (format "%s/browse/%s" jiraffe-base-url key))

(defun jiraffe--headers ()
  (jiraffe--filter-nils
   (when jiraffe-authenticate
     `("Authntication" . ,(concat "Basic " (jiraffe--read-secret))))
   '("Content-type" . "application/json")))

(defun jiraffe--encode-get-params (params)
  "Expecting params to be alist"
  (s-join "&" (--map (format "%s=%s" (car it) (cdr it)) params)))

(defun jiraffe--quote-sql-string (x)
  (s-replace "\"" "\\\"" x))

(defun jiraffe--rest-url-with-get-params (mini-url &optional params)
  (if params
      (let ((sep (if (equal "/" (s-right 1 mini-url)) "?" "/?")))
          (concat (jiraffe--rest-url mini-url) sep (jiraffe--encode-get-params params)))
    (jiraffe--rest-url mini-url)))

(defun jiraffe--parse-http-response-to-json (buffer)
  "Maps buffer -> json"
  ;; This is tricky moment: url-retreive doesn't detect utf-8 response
  ;; automatically (emacs shows characters as \342\240... So we "reuse"
  ;; restclient-decode-response to get utf-8 buffer
  (with-current-buffer
      (restclient-decode-response buffer (get-buffer-create "*jiraffe-data*") t)
    (progn
      (goto-char (point-min))
      ;; Skip headers
      (re-search-forward "^$")
      ;; Parse rest of buffer as json
      (let* ((json-object-type 'alist)
             (json-array-type 'vector))
        (json-read)))))

(defun jiraffe--maybe-dump-responce (result)
  (when jiraffe--debug-save-response-to-file
    (f-write-text (json-encode result) 'utf-8 jiraffe--debug-save-response-to-file)))

(defun jiraffe--add-parsing-to-callback (callback)
  (lambda (status)
    (-when-let (err (plist-get status :error))
      (goto-char (point-min))
      (let ((first-line (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position))))
        (switch-to-buffer (current-buffer))
        (signal (car err) (list first-line))))
    (let ((res (jiraffe--parse-http-response-to-json (current-buffer))))
      (jiraffe--maybe-dump-responce res)
      (funcall callback res))))

(defun jiraffe--retrieve-common-debug (method mini-url callback &optional params)
  method   ;; hide unused parameter warning
  mini-url
  callback
  params
  (funcall callback
           (json-read-from-string (f-read-text jiraffe--debug-read-response-from-file))))

(defun jiraffee--with-url-headers (headers callback)
  (let (url-request-extra-headers '())
    (restclient-restore-header-variables)

    (dolist (header (jiraffe--headers))
      (let* ((mapped (assoc-string (downcase (car header))
                                   '(("from" . url-personal-mail-address)
                                     ("accept-encoding" . url-mime-encoding-string)
                                     ("accept-charset" . url-mime-charset-string)
                                     ("accept-language" . url-mime-language-string)
                                     ("accept" . url-mime-accept-string)))))

        (if mapped
            (set (cdr mapped) (cdr header))
          (setq url-request-extra-headers (cons header url-request-extra-headers)))))

    (funcall callback)))

(defun jiraffe--retrieve-common-normal (method mini-url callback &optional params)
  (jiraffee--with-url-headers
   (jiraffe--headers)
   (lambda ()
     (let ((url-request-method method)
           (url-request-data '())
           (full-url '()))

       (when (and (equal method "POST") params)
         (setq url-request-data (json-encode params)))
       (if (equal method "GET")
           (setq full-url (jiraffe--rest-url-with-get-params mini-url params))
         (setq full-url (jiraffe--rest-url mini-url)))

       (url-retrieve full-url (jiraffe--add-parsing-to-callback callback))))))

(defun jiraffe--retrieve-common (method mini-url callback &optional params)
  (if jiraffe--debug-read-response-from-file
      (jiraffe--retrieve-common-debug method mini-url callback params)
    (jiraffe--retrieve-common-normal method mini-url callback params)))

(defun jiraffe-get (mini-url callback &optional params)
  "Retrieves data from jira asynchronously using GET request.
Calls callback only in case of success with json parsed into
elisp alists and vectors. Gives no guarantees about about saving
excursion and current buffer."
  (jiraffe--retrieve-common "GET" mini-url callback params))

(defun jiraffe-get-signal (mini-url &optional params) (lifted:signal
   (lambda (subscriber)
     (jiraffe-get mini-url
               (lambda (x) (funcall subscriber :send-next x))
               params))))

(defun jiraffe-post (mini-url callback &optional body-params)
  "Retrieves data from jira asynchronously using POST request.
Calls callback only in case of success with json parsed into
elisp alists and vectors. body-params are encoded into json.
Gives no guarantees about about saving excursion and current
buffer."
  (jiraffe--retrieve-common "POST" mini-url callback body-params))

(defun jiraffe-post-signal (mini-url &optional params)
  (lifted:signal
   (lambda (subscriber)
     (jiraffe-post mini-url
                (lambda (x) (funcall subscriber :send-next x))
                params))))

(defun jiraffe-jql-filter-signal (jql)
  "Sequence of mini-issues. Currently can not fetch result in
several requests so it returns only first 'page' which is
requested to be of size 1500"
  (lifted:map
   #'jiraffe--minify-jiraffe-list
   (jiraffe-post-signal "search" `(("jql" . ,jql)
                                ("maxResults" . 1500)))))

(defun jiraffe--minify-jiraffe-list (xs)
  (-map #'jiraffe--minify-jira (jiraffe--at 'issues xs)))

(defun jiraffe--minify-jira (x)
  "Let call result 'issue' which is simplified representation of jira"
  (list
   (assoc 'key x)
   (cons 'labels      (jiraffe--at '(fields labels) x))
   (cons 'project     (jiraffe--at '(fields project name) x))
   (cons 'project_key (jiraffe--at '(fields project key) x))
   (cons 'issue_type  (jiraffe--at '(fields issuetype name) x))
   (cons 'summary     (jiraffe--at '(fields summary) x))))

(defun jiraffe--issue-caption (issue)
  (let ((key     (jiraffe--at 'key issue))
        (summary (jiraffe--at 'summary issue)))
    (format "[[%s][%s]] %s" (jiraffe--issue-browse-url key) key summary)))

(defun jiraffe--issue-to-org-caption (x nesting-level)
  (format "%s %s" (make-string nesting-level ?*) (jiraffe--issue-caption x)))

(defun jiraffe--issue-to-org-text (x nesting-level)
  (jiraffe--issue-to-org-caption x nesting-level))

(defun jiraffe--issue-key-from-text (text)
  (--when-let (s-match "\\([[:upper:]]+-[[:digit:]]+\\)[[:space:]]*$" text)
    (-last-item it)))

(defun jiraffe--find-issue-in-buffer (issue buffer)
  (let ((pattern (format "*+ \\w+ %s" (regexp-quote (jiraffe--issue-caption issue)))))
    (save-excursion
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward pattern '() t) ;; suppress error
          t)))))

(defun jiraffe--get-flat-issue-files ()
  (--filter (s-ends-with? ".org" it)
   (-flatten
    (--map (if (f-directory? it) (f-files it) it) jiraffe-issue-files))))

(defun jiraffe--find-issue-in-buffer-and-files (issue buffer)
  (or
   (jiraffe--find-issue-in-buffer issue buffer)
   (--any (with-current-buffer (find-file-noselect it)
            (jiraffe--find-issue-in-buffer issue (current-buffer)))
          (jiraffe--get-flat-issue-files))))

(defun jiraffe--my-issues-signal ()
  (jiraffe-jql-filter-signal jiraffe-my-issues-jql))

(defun jiraffe--kill-line ()
  "Kill line without kill ring"
  (let ((beg (point)))
    (forward-line 1)
    (delete-region beg (point))))

(defun jiraffe--find-heading-nesting-level ()
  (save-match-data
    (save-excursion
      (--if-let (re-search-backward "^\\(\*+\\) " '() t)
          (length (match-string 1))
        0))))

(defun jiraffe--insert-jiras-main-part (issues-list &optional force)
  "Inserts text representation at point in current buffer.
Returns count of inserted and filtered tasks as cons. force
parameter disables filtering."
  (let ((nesting-level (1+ (jiraffe--find-heading-nesting-level)))
        (good-cnt 0)
        (bad-cnt  0))
    (-each issues-list
        (lambda (issue)
          (if (and (not force)
                   (jiraffe--find-issue-in-buffer-and-files issue (current-buffer)))
              (incf bad-cnt)
            (when (> good-cnt 0) (insert "\n"))
            (incf good-cnt)
            (insert (jiraffe--issue-to-org-text issue nesting-level)))))
    (message "Done (inserted %s%s)"
             good-cnt
             (if (> bad-cnt 0) (format "; skipped %s" bad-cnt) ""))))

(defun jiraffe--insert-jiras (filter-signal &optional force)
  "This function could be described using pseudo code: signal
provides issues => insert each issue into current pos. Tricky
moment is asynchronous nature of data retrieval. Instead of
blocking we remember where to place result. We mark this place in
buffer by magic string. Later magic string is replaced by result.
User can remove magic string to cancel operation."
  (goto-char (line-beginning-position))
  (insert (format "%s\n\n" jiraffe-pending-request-placeholder))
  (forward-line -2)
  (let ((buffer (current-buffer))
        (placeholder-position (point)))
    (funcall filter-signal
             :subscribe-next
             (lambda (issues)
               (with-current-buffer buffer
                 (save-excursion
                   (goto-char placeholder-position)
                   (jiraffe--kill-line)
                   (jiraffe--insert-jiras-main-part issues force)))
               (goto-char placeholder-position)))))

(defun jiraffe--filters-helm-sources ()
  (jiraffe--filter-nils
   (when jiraffe-quick-filters
     (helm-build-sync-source "Quick filters"
       :candidates jiraffe-quick-filters
       :fuzzy-match t))))

(defun jiraffe--ask-single-replacement (hole)
  (cons (format "{%s}" hole)
        (helm-comp-read (format "%s: " hole)
                        jiraffe--templates-history
                        :input-history 'jiraffe--templates-history
                        :action #'jiraffe--quote-sql-string)))

(defun jiraffe--populate-template (str)
  "Replaces whildcards like {name} with strings obtained
interactively from user"
  (let* ((holes (-uniq (-flatten (-map '-last-item (s-match-strings-all "{\\([^}]+\\)}" str)))))
         (replacements (-map #'jiraffe--ask-single-replacement holes)))
    (if replacements
        (s-replace-all replacements str)
      str)))

(defun jiraffe--ask-jql-filter ()
  (-when-let (jql-template (helm :sources (jiraffe--filters-helm-sources) :buffer "*jiraffe-filters*"))
      (jiraffe--populate-template jql-template)))

(defun jiraffe--convert-text-to-issue-signal (text)
  (let ((jql (--if-let (jiraffe--issue-key-from-text text)
                 (format "key = %s" it)
               (format "summary ~ \"%s\"" (jiraffe--quote-sql-string text)))))
    (funcall (jiraffe-jql-filter-signal jql)
             :map (lambda (x) (-take 1 x)))))

;;;###autoload
(defun jiraffe-insert-filter-result-here (&optional arg)
  "Prefix argument disables filtering."
  (interactive "P")
  (--when-let (jiraffe--ask-jql-filter)
    (jiraffe--insert-jiras (jiraffe-jql-filter-signal it) (consp arg))))

;;;###autoload
(defun jiraffe-show-filter-result ()
  (interactive)
  (--when-let (jiraffe--ask-jql-filter)
    (with-current-buffer (get-buffer-create (generate-new-buffer-name "*Jql result*"))
      (jiraffe--insert-jiras (jiraffe-jql-filter-signal it) t)
      (org-mode)
      (switch-to-buffer (current-buffer)))))

;;;###autoload
(defun jiraffe-insert-my-issues-here (&optional arg)
  "Prefix argument disables filtering."
  (interactive "P")
  (jiraffe--insert-jiras (jiraffe--my-issues-signal) (consp arg)))

;;;###autoload
(defun jiraffe-yank-issue ()
  "Looks into kill ring and decides if is contains jira ticket name,
link or simply some text. Request corresponding issue and inserts
in current position. Algorithm for analysis of kill ring is
simple: line which end with [letters]-[numbers] is link or ticket
name, in both cases we can request issues with that key.
Otherwise - this is text and it is part of summary. In case of
multiline input it looks only at first line."
  (interactive)
  (-when-let (first-killed-line
              (-first (lambda (x) (not (s-blank? x))) (s-split "\n" (car kill-ring))))
    (jiraffe--insert-jiras (jiraffe--convert-text-to-issue-signal first-killed-line))))

(provide 'jiraffe)

;;; jiraffe.el ends here
