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

;;;###autoload
(defun benedict/init! ()
  "Initialize benedicts configuration."
  (interactive)
  (benedict-config/init!))

;;;;;;;
;; JIRA
(require 'bd-jira-issue)

;;;###autoload
(defun benedict/list-issues ()
  "Display all active Engine issues in another buffer."
  (interactive)
  (benedict/init!)
  (benedict-jira-issue/list
   "project = XDRRESP and component = Engine and status != Done"))

;;;###autoload
(defun benedict/issue-detail (issue-key)
  "Get details for ISSUE-KEY as plist."
  (benedict/init!)
  (benedict-jira-issue/detail issue-key))

;;;###autoload
(defun benedict/issue-create ()
  "Create a new jira issue."
  (interactive)
  (benedict/init!)
  (benedict-jira-issue/create))

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
	(t (message (format "Cannot update %s with unrecognized field %s"
			    issue-key field)))))

;;;;;;;
;;; Git

(require 'bd-github-branch)

;;;###autoload
(defun benedict/spinoff-branch ()
  "Spinoff a branch prompting for an issue or finding one at point."
  (interactive)
  (let ((issue-key nil))
    (when (string-equal major-mode "org-mode")
      (setq issue-key (org-entry-get nil "ID" 'select)))
    (unless issue-key
      (setq issue-key (read-string "issue: ")))
    (bd-github-branch/spinoff-from-key issue-key)))

(provide 'benedict)
;;; benedict.el ends here
