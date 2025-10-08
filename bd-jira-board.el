;;; bd-jira-board.el --- Manage jira within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 23 May 2024
;; Homepage: N/A
;; Keywords: jira, atlassian
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 0.0.1

;;; Commentary:

;; Provide utilities for interacting with jira boards
;;
;; Comparison:
;; Try to be lighter weight and less opinionated than
;; org jira

;;; Code:

(require 'bd-jira-request)
(require 'subr-x)
(require 'json)

;;;;;;;;;;
;; sprints


(defun bd-jira-board--parse-board (board)
  "Parse JIRA board data into an organized list structure.
BOARD is an alist containing Jira board information.  Returns a plist with
standardized keys :name, :number, :url, :project, and :type with their
corresponding values extracted from the board data."
  (let ((name (alist-get 'name board))
	(number (alist-get 'id board))
	(type (alist-get 'type board))
	(url (alist-get 'self board))
	(project (alist-get 'projectName (alist-get 'location board))))
    (list :name name :number number :url url :project project :type type)))

(defun bd-jira-board/list (&optional project name)
  (let ((parameters '()))
    (when project (push (cons 'projectKeyOrId project) parameters))
    (when name (push (cons 'name name) parameters))
    (let ((response (bd-jira-agile-request "board" :parameters parameters)))
      (mapcar #'bd-jira-board--parse-board (alist-get 'values response)))))

(defun bd-jira-board--fetch-jira-sprints (board-ids)
  (mapcan
   (lambda (board-id)
     (let ((path (format "board/%s/sprint" board-id)))
       (alist-get 'values (bd-jira-agile-request path))))
   board-ids))

(defun bd-jira-board--->sprint (jira-sprint-spec)
  "Create a plist of JIRA-SPRINT-SPEC."
  (let ((id (alist-get 'id jira-sprint-spec))
	(name (alist-get 'name jira-sprint-spec))
	(state (alist-get 'state jira-sprint-spec)))
    (list :id id :name name :state state)))

(defun bd-jira-board/active-sprints (board-ids)
  "Fetch any active sprints for BOARD-IDS."
  (thread-last
    (bd-jira-board--fetch-jira-sprints board-ids)
    (seq-filter (lambda (sprint) (equal (alist-get 'state sprint) "active")))
    (mapcar #'bd-jira-board--->sprint)))

(defun bd-jira-board/sprints (board-ids)
  "Fetch any sprints for BOARD-IDS."
  (thread-last
    (bd-jira-board--fetch-jira-sprints board-ids)
    (mapcar #'bd-jira-board--->sprint)))

(defun bd-jira-board/backlog (board-id)
  "Fetch any backlog issues for BOARD-ID."
  (bd-jira-agile-request
   (format "board/%s/backlog" board-id)
   :headers '(("Accept" . "application/json")
	      ("Content-Type" . "application/json"))
   :parameters (list (cons 'maxResults 400))))

(defun bd-jira-sprint/issues (sprint)
  "Fetch any issues in SPRINT."
  (bd-jira-agile-request
   (format "sprint/%s/issue" sprint)
   :headers '(("Accept" . "application/json")
	      ("Content-Type" . "application/json"))
   :parameters (list (cons 'maxResults 400))))

(defun bd-jira-sprint/add-issue (issue sprint)
  "Add ISSUE to SPRINT."
  (bd-jira-agile-request
   (format "sprint/%s/issue" sprint)
   :type "POST"
   :headers '(("Accept" . "application/json")
	      ("Content-Type" . "application/json"))
   :data (json-encode (list (cons 'issues (list issue))))))

(cl-defun bd-jira-board/issues (board &key assignee)
  "Retrieve issues from the specified Jira BOARD.
BOARD should be a board ID (numeric string or number).
Returns up to 400 issues from the board."
  (let ((headers (list (cons "Accept"  "application/json")
		       (cons "Content-Type"  "application/json"))))
    (when assignee
      (push (cons "jql" (format "accountId=%s" assignee)) headers))
    (bd-jira-agile-request
     (format "board/%s/issue" board)
     :headers headers
     :parameters (list (cons 'maxResults 400)))))

(provide 'bd-jira-board)
;;; bd-jira-board.el ends here
