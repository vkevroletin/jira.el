(require 'url)
(require 'json)

(defun my-url-http-get (url)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authntication" . ,(concat "Basic " (user-config/get-jira-secret))))))
    (url-retrieve url 'my-switch-to-url-buffer)))

(defun my-kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defun my-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

(my-url-http-get "https://jira.rhonda.ru/rest/api/latest/filter/favourite")
