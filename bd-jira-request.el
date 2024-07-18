;;; bd-jira-request.el --- Manage jira within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 23 May 2024
;; Homepage: N/A
;; Keywords: jira, atlassian
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 0.0.1

;;; Commentary:

;; Provide utilities for interacting with jira
;;
;; Comparison:
;; Try to be lighter weight and less opinionated than
;; org jira

;;; Code:
(require 'bd-jira-config)

(cl-defun bd-jira-request (path &key type headers parameters data)
  "Make a get request for PATH.
Optionally specify TYPE - the default is GET.
Optionally specify HEADERS, PARAMETERS or DATA."
  (when *bd-jira-config/config*
    (cl-destructuring-bind (&key domain token user &allow-other-keys)
	*bd-jira-config/config*
      (let ((auth-string (base64-encode-string (format "%s:%s" user token) t)))
	(push (cons "Authorization" (format "Basic %s" auth-string)) headers))
      (let ((request-type (or type "GET")))
	(request-response-data
	 (request
	     (format "https://%s/rest/api/2/%s" domain path)
	     :type request-type
	     :sync t
	     :parser 'json-read
	     :headers headers
	     :data data
	     :params parameters))))))

;;(defvar *metadata*
;;  (bd-jira-request/get
;;   "issue/createmeta/XDR/issuetypes"
;;   :headers
;;   '(("Content-Type" . "application/json")
;;     ("Accept" . "application/json"))))
;;
;;(mapcar (lambda (type) (alist-get 'name type)) (alist-get 'issueTypes *metadata*))

;;(bd-jira-request
;; "issue"
;; :type "POST"
;; :headers (list (cons "Content-Type" "application/json")
;;		(cons "Accept" "application/json"))
;; :data
;; (json-encode
;;  (list (cons 'fields (list
;;		       (cons 'project (list (cons 'key "XDR")))
;;		       (cons 'components
;;			     (list (list (cons 'name "Engine"))))
;;		       (cons 'description "In January a lot of data that didn't belong to XDR was dumped into the PROD Conure database. A script has been worked out to delete the data: https://github.com:advthreat/preen. When we have a way of performing bulk cleaning operations on the Conure database, we can execute that script.")
;;		       (cons 'summary "Clean Non-XDR data from Conure")
;;		       (cons 'issuetype (list (cons 'name "Task")))
;;		       (cons 'parent (list (cons 'key "XDR-3067"))))))))


(provide 'bd-jira-request)
;;; bd-jira-request.el ends here
