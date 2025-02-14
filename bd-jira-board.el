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
(require 'bd-jira-request)
(require 'subr-x)
(require 'json)

;;;;;;;;;;
;; sprints

(defun bd-jira-board/list (&optional project name)
  (let ((parameters '()))
    (when project (push (cons 'projectKeyOrId project) parameters))
    (when name (push (cons 'name name) parameters))
    (bd-jira-agile-request "board" :parameters parameters)))

(defun bd-jira-board--fetch-jira-sprints (board-ids)
  (mapcan
   (lambda (board-id)
     (let ((path (format "board/%s/sprint" board-id)))
       (alist-get 'values (bd-jira-agile-request path))))
   board-ids))

(defun bd-jira-board--->sprint (jira-sprint-spec)
  (let ((id (alist-get 'id jira-sprint-spec))
	(name (alist-get 'name jira-sprint-spec)))
    (list :id id :name name)))

(defun bd-jira-board/active-sprints (board-ids)
  "Fetch any active sprints for BOARD-IDS"
  (thread-last
    (bd-jira-board--fetch-jira-sprints board-ids)
    (seq-filter (lambda (sprint) (equal (alist-get 'state sprint) "active")))
    (mapcar #'bd-jira-board--->sprint)))

(defun bd-jira-board/sprints (board-ids)
  "Fetch any sprints for BOARD-IDS"
  (thread-last
    (bd-jira-board--fetch-jira-sprints board-ids)
    (mapcar #'bd-jira-board--->sprint)))

(defun bd-jira-sprint/issues (sprint)
  "Fetch any issues in SPRINT."
  (bd-jira-agile-request
   (format "sprint/%s/issue" sprint)
   :headers '(("Accept" . "application/json")
	      ("Content-Type" . "application/json"))
   :parameters (list (cons 'maxResults 400))))

(defun bd-jira-sprint/add-issue (issue sprint)
  "Add ISSUES to SPRINT."
  (bd-jira-agile-request
   (format "sprint/%s/issue" sprint)
   :type "POST"
   :headers '(("Accept" . "application/json")
	      ("Content-Type" . "application/json"))
   :data (json-encode (list (cons 'issues (list issue))))))

(provide 'bd-jira-board)
;;; bd-jira-board.el ends here
