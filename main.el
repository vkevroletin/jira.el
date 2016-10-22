;; -*- lexical-binding: t -*-
(require 'request)
(require 'json)
(require 'f)
(require 'dash)

(setq request-backend 'url-retrieve)

(defcustom jira-base-url "https://jira.rhonda.ru"
  "Url like https://jira.rhonda.ru"
  :group 'restclient
  :type 'string)

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

;;
;; SCRATCH
;;

(defun example-01 ()
  (jira-get "issue/RH-512"
            (lambda (x)
              (pp x)
              (f-write-text (json-encode x) 'utf-8 "data.txt"))
            '(("filter" . "12953"))))

(defun exapmle-02 ()
  (jira-post "search"
             (lambda (x)
               (pp x)
               (f-write-text (json-encode x) 'utf-8 "data.txt"))
             '(("jql" . "project = \"Rhonda Internal\" AND \"Project Name\" = Automation AND status not in (closed, resolved) ORDER BY cf[10802] ASC, cf[10102] ASC, priority DESC, key ASC"))))

(defun example-03 ()
  (setq data (f-read-text "data.txt"))
  (setq obj
        (let ((json-object-type 'alist)
              (json-array-type 'vector))
          (json-read-from-string data)))

  (defun jira--minify-jira (x)
    (list "key" (alist-get x "key")))

  (mapcar #'jira--minify-jira data)

  (defun dp (data)
    (let ((buffer (get-buffer-create "dbd"))
          (json-encoding-pretty-print t))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (json-encode data))
        (switch-to-buffer buffer)
        ))))
