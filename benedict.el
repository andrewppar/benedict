;;; benedict.el --- Manage External Tasks -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 23 May 2024
;; Homepage: N/A
;; Keywords: jira, atlassian, github, git
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 0.0.1

;;; Commentary:

;; I have a lot of different task software that I have to interact with, e.g.
;; JIRA, Github, etc.
;;
;; These are kind of integrated with forges that I use, e.g. github.  This is my solution to
;; having one place to deal with all of that.
;;
;; Of course, this is in Emacs.  It attempts to collect all the things that I might want
;; to do with something like this in a single place under a single way of operating.
;;

;;; Code:
(require 'benedict-config)
(require 'cl-lib)

;;;###autoload
(defun benedict/init! ()
  "Initialize benedicts configuration."
  (interactive)
  (benedict-config/init!))

;;;;;;;
;; JIRA
(require 'bd-jira-issue)
(require 'bd-jira-board)
(require 'bd-jira-view)

;;;###autoload
(defun benedict/list-issues (query)
  "Display all active Engine issues in another buffer."
  (interactive "sJQL Query: ")
  (benedict/init!)
  (bd-jira-view/issues (benedict-jira-issue/list query)))

;;;###autoload
(defun benedict/issue-search (query-text)
  "Get JIRA issues satisfying QUERY-TEXT."
  (interactive "sSearch Text: ")
  (benedict/init!)
  (bd-jira-view/issues
   (bd-jira-issue/get-issues-from-query query-text)))

;;;###autoload
(defun benedict/sprint (sprint-id)
  "Show all issues in a sprint."
  (interactive)
  (benedict/init!)
  (bd-jira-view/issues
   (bd-jira-issue--parse-issue-response
    (bd-jira-sprint/issues sprint-id))))

;;;###autoload
(defun benedict/issue-detail (issue-key)
  "Get details for ISSUE-KEY as plist."
  (interactive "sIssue Key: ")
  (benedict/init!)
  (benedict-jira-view/issue-detail issue-key))

;;;###autoload
(cl-defun benedict/issue-create (&key project component &allow-other-keys)
  "Create a new jira issue.
Optionally specify COMPONENT and PROJECT as keywords."
  (interactive)
  (benedict/init!)
  (benedict-jira-issue/create project component))

;;;###autoload
(defun benedict/issue-update (issue-key field &rest values)
  "Update ISSUE-KEY at FIELD with VALUES."
  (interactive)
  (benedict/init!)
  (cond ((equal field :status)
	 (bd-jira-issue/update-status issue-key (car values)))
	((equal field :link)
	 (bd-jira-issue/add-link issue-key (car values) (cadr values)))
	((equal field :assign)
	 (bd-jira-issue/assign issue-key (car values)))
	((equal field :parent)
	 (bd-jira-issue/add-parent issue-key (car values)))
	((equal field :sprint)
	 (bd-jira-sprint/add-issue issue-key (car values)))
	((equal field :label)
	 (bd-jira-issue/add-label issue-key (car values)))
	(t
	 (message
	  (format "Cannot update %s with unrecognized field %s"
		  issue-key field)))))


(provide 'benedict)
;;; benedict.el ends here
