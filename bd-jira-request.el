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
(require 'cl-lib)
(require 'request)

(defun bd-jira-request--request
    (jira-segment path rest-method headers parameters data)
  "Make a request to JIRA-SEGMENT - a hard coded endpoint specified by
:agile or :standard.
Use REST-METHOD at PATH for that endpoing. Passing along HEADERS, PARAMETERS,
and DATA."
  (when *bd-jira-config/config*
    (cl-destructuring-bind (&key domain token user &allow-other-keys)
	*bd-jira-config/config*
      (let ((auth-string (base64-encode-string (format "%s:%s" user token) t)))
	(push (cons "Authorization" (format "Basic %s" auth-string)) headers))
      (let ((rest-method (or rest-method "GET"))
	    (url (cond ((eq jira-segment :standard)
			(format "https://%s/rest/api/2/%s" domain path))
		       ((eq jira-segment :agile)
			(format "https://%s/rest/agile/1.0/%s" domain path))
		       (t
			(format "https://%s/rest/api/2/%s" domain path)))))
	(request-response-data
	 (request url
	     :type rest-method
	     :sync t
	     :parser 'json-read
	     :headers headers
	     :data data
	     :params parameters))))))

(cl-defun bd-jira-request (path &key type headers parameters data)
  "Make a get request for PATH to JIRAs standard API.
Optionally specify REST-METHOD - the default is GET.
Optionally specify HEADERS, PARAMETERS or DATA."
  (bd-jira-request--request :standard path type headers parameters data))

(cl-defun bd-jira-agile-request (path &key type headers parameters data)
  "Make a get request for PATH to JIRAs agile API.
Optionally specify TYPE - the default is GET.
Optionally specify HEADERS, PARAMETERS or DATA."
  (bd-jira-request--request :agile path type headers parameters data))

(provide 'bd-jira-request)
;;; bd-jira-request.el ends here
