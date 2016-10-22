;; -*- lexical-binding: t -*-
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

(defcustom jira--debug-save-response-to-file '()
  "Path to file"
  :group 'jira
  :type 'string)

(defcustom jira--debug-read-response-from-file '()
  "Path to file"
  :group 'jira
  :type 'string)

(defun jira--at-helper (key data)
  (if (numberp key)
      (if (>= (length data) key)
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

(defun jira--rest-url (x)
  (format "%s/rest/api/latest/%s" jira-base-url x))

(defun jira--headers ()
  `(("Authntication" . ,(concat "Basic " (user-config/get-jira-secret)))
    ("Content-type" . "application/json")))

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
  (lifted:map
   #'jira--minify-jira-list
   (jira-post-signal "search" `(("jql" . ,jql)))))

(defun jira--minify-jira-list (xs)
  (-map #'jira--minify-jira (jira--at 'issues xs)))

(defun jira--minify-jira (x)
  `(,(assoc 'key x)
    (labels      ,(jira--at '(fields labels) x))
    (project     ,(jira--at '(fields project name) x))
    (project_key ,(jira--at '(fields project key) x))
    (issue_type  ,(jira--at '(fields issuetype name) x))
    (summary     ,(jira--at '(fields summary) x))))

(defun jira--mini-jira-to-org-task (x)
  (s-join "\n"
          (list
           (format "* MAYBE %s" (jira--at 'summary x)))))
