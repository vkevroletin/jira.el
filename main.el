;; -*- lexical-binding: t -*-
(require 'request)
(require 'json)
(require 'f)
(require 'dash)

(defcustom jira-base-url "https://jira.rhonda.ru"
  "Url like https://jira.rhonda.ru"
  :group 'restclient
  :type 'string)

(defun jira--at (keys alist)
  (if (not (consp keys))
      (cdr (assoc keys alist))
    (-let (((x . xs) keys))
      (if xs
          (jira--at xs (cdr (assoc x alist)))
        (cdr (assoc x alist))))))

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

(defun jira--run-url-retreive(full-url callback &optional body-params)
  (url-retrieve full-url
                (lambda (status)
                  (goto-char (point-min))
                  (-when-let (err (plist-get status :error))
                    (let ((first-line (buffer-substring-no-properties (line-beginning-position)
                                                                      (line-end-position))))
                      (signal (car err) (list first-line))))
                  ;; Skip headers
                  (forward-paragraph)
                  ;; Parse rest of buffer as json
                  (let* ((json-object-type 'alist)
                         (json-array-type 'vector)
                         (js (json-read)))
                    (funcall callback js)))))

(defun jira-get (mini-url callback &optional params)
  "Retrieves data from jira asynchronously using GET request.
Calls callback only in case of success with json parsed into
elisp alists and vectors. Gives no guarantees about about saving
excursion and current buffer."
  (let ((url-request-method "GET")
        (url-request-extra-headers (jira--headers)))
    (jira--run-url-retreive (jira--rest-url-with-get-params mini-url params) callback)))

(defun jira-post (mini-url callback &optional body-params)
  "Retrieves data from jira asynchronously using POST request.
Calls callback only in case of success with json parsed into
elisp alists and vectors. body-params are encoded into json.
Gives no guarantees about about saving excursion and current
buffer."
  (let ((url-request-method "POST")
        (url-request-extra-headers (jira--headers))
        (url-request-data (if body-params (json-encode body-params) '())))
    (jira--run-url-retreive (jira--rest-url mini-url) callback)))

(defun jira--minify-jira (x)
  `(,(assoc 'key x)
    (labels      ,(jira--at '(fields labels) x))
    (project     ,(jira--at '(fields project name) x))
    (project_key ,(jira--at '(fields project key) x))
    (issue_type  ,(jira--at '(fields issuetype name) x))
    (summary     ,(jira--at '(fields summary) x))))
