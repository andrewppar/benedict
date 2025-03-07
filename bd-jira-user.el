;;; bd-jira-user.el --- Manage jira within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 23 May 2024
;; Homepage: N/A
;; Keywords: jira, atlassian
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 0.0.1

;;; Commentary:

;; Provide a configuration for jira requests
;;
;; Comparison:
;; Try to be lighter weight and less opinionated than
;; org jira

;;; Code:
(require 'bd-jira-request)

(defun bd-jira-user/search (search-term)
  "Get user information from SEARCH-TERM."
  (let ((response (bd-jira-request "user/search"
			 :headers '(("Content-Type" . "application/json")
				    ("Accept" . "application/json"))
			 :parameters (list (cons "query" search-term)))))
    (if (> (length response) 0)
	(aref response 0)
      (message (format "Search term \"%s\" found no JIRA users." search-term)))))

(defun bd-jira-user/from-id (id)
  (bd-jira-request "user"
		   :headers '(("Content-Type" . "application/json")
			      ("Accept" . "application/json"))
		   :parameters (list (cons 'accountId id))))


(defun bd-jira-user/insert-ref (search-term)
  "Insert account associated with SEARCH-TERM in jira markdown."
  (interactive (list (read-string "search by: ")))
  (let* ((person (bd-jira-user/search search-term))
	 (accountid (alist-get 'accountId person))
	 (name (alist-get 'displayName person))
	 (email (alist-get 'emailAddress person))
	 (enter? (y-or-n-p (format "Found %s (%s).  Is this correct? " name email))))
    (when enter?
      (insert (format "[~accountid:%s]" accountid)))))

(defun bd-jira-user/id (search-term)
  "Get the account id associated with SEARCH-TERM."
  (alist-get 'accountId (bd-jira-user/search search-term)))

(defun bd-jira-user/set-user ()
  "Set the configuration account id."
  (when-let ((email (bd-jira-config/get :user)))
    (unless (bd-jira-config/get :account-id)
      (bd-jira-config/set :account-id (bd-jira-user/id email)))))


(provide 'bd-jira-user)
;;; bd-jira-user.el ends here
