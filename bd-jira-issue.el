;;; bd-jira-issue.el --- Manage jira within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 23 May 2024
;; Homepage: N/A
;; Keywords: jira, atlassian
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 0.0.1

;;; Commentary:

;; Create, Edit, and Delete Issues

;;; Code:
(require 'bd-jira-request)
(require 'bd-jira-user)
(require 'bd-jira-config)

;;;;;;;;;;;;;
;;; Serialize


;;; TODO: This could be a little more dynamic
;; e.g.
;;(defvar *bd-jira-issue/issue-types*
;;  (thread-last
;;    (bd-jira-request/get
;;     "issue/createmeta/XDR/issuetypes"
;;     :headers
;;     '(("Content-Type" . "application/json")
;;       ("Accept" . "application/json")))
;;    (alist-get 'issueTypes)
;;    (mapcar
;;     (lambda (it) (alist-get 'name it)))))

(defconst *bd-jira-issue/priorities*
  ;; (bd-jira-request "priority")
  '("Critical" "Major" "Blocker" "Highest" "High" "TBD"
    "Normal" "Medium"
    "Low" "Lowest" ))

(defconst *bd-jira-issue/transitions*
  ;;(mapcar
  ;; (lambda (transition)
  ;;   (cons
  ;;    (alist-get 'name transition)
  ;;    (string-to-number (alist-get 'id transition))))
  ;; (alist-get
  ;;  'transitions
  ;;  (bd-jira-request "issue/XDR-3078/transitions")))
  '(("Backlog" . 21) ("Selected for Development" . 31)
    ("In Progress" . 41)
    ("QA" . 61) ("Ready for QA" . 91) ("Ready For Acceptance" . 101) ("In Review" . 111)
    ("Done" . 121)))

(defconst *bd-jira-issue/statuses*
  '("Open"
    "Backlog"
    "Selected for Development"
    "In Progress"
    "In Review"
    "Ready for QA"
    "Ready For Acceptance"
    "Done"))

(defconst *bd-jira-issue/link-types*
  ;;(thread-last
  ;;  (bd-jira-request "issueLinkType"
  ;;		     :headers '(("Content-Type" . "application/json")
  ;;				("Accept" . "application/json")))
  ;;  (alist-get 'issueLinkTypes)
  ;;  (mapcar
  ;;   (lambda (link-type) (alist-get 'name link-type))))
  '("Blockers" "Blocks"
    "Bonfire Testing"
    "Cloners"
    "Defect"
    "Dependency"
    "Developer Escalations"
    "Duplicate"
    "Issue split"
    "Post-Incident Reviews"
    "Problem/Incident"
    "Related" "Relates"
    "Test" "Test (migrated)"))

;;; Custom Deserialization
;; Example set this const whereever with whatever values
(defun bd-jira-issue--parse-sprints (sprint-array)
  "Parse an array of jira sprint representations into the internal one."
  (mapcar
   (lambda (sprint)
     (let ((name (alist-get 'name sprint))
	   (state (alist-get 'state sprint))
	   (goal (alist-get 'goal sprint))
	   (start (alist-get 'startDate sprint))
	   (end (alist-get 'endDAte sprint)))
       (list :name name
	     :state state
	     :goal goal
	     :start start
	     :end end)))
   sprint-array))

(defconst *bd-jira-issue/custom-fields*
  (list
   (cons 'customfield_10020
	 (list :name :sprints :parse-fn #'bd-jira-issue--parse-sprints))))

(defun bd-jira-issue--parse-comments (fields)
  "Get comments if there are any from FIELDS."
  (let* ((comment-vec (alist-get 'comments (alist-get 'comment fields)))
	 (result '())
	 (idx 0))
    (while (< idx (length comment-vec))
      (let* ((comment (aref comment-vec idx))
	     (author (alist-get 'displayName (alist-get 'author comment)))
	     (created (alist-get 'created comment))
	     (body (alist-get 'body comment)))
	(push (list :comment body :author author :created created) result))
      (setq idx (+ idx 1)))
    result))

;;; todo: need to select relation name based on incoming or outgoing entity

(defun bd-jira-issue--update-map (map key value function)
  "Update MAP at KEY with VALUE using FUNCTION."
  (let* ((entry (alist-get key map nil t #'equal))
	 (new-result (list (cons key (funcall function value entry)))))
    (dolist (entry map)
      (unless (equal (car entry) key)
	(push entry new-result)))
    new-result))

(defun bd-jira-issue--parse-relations (fields)
  "Get relations if there are any in FIELDS."
  (let* ((links (alist-get 'issuelinks fields))
	 (result '())
	 (idx 0))
    (while (< idx (length links))
      (let* ((entry (aref links idx))
	     (keys (mapcar #'car entry))
	     (relation-type (alist-get 'type entry))
	     (relation nil)
	     (relatum nil))
	(cond ((member 'inwardIssue keys)
	       (setq relation (alist-get 'inward relation-type)
		     relatum (alist-get 'key (alist-get 'inwardIssue entry))))
	      ((member 'outwardIssue keys)
	       (setq relation (alist-get 'outward relation-type)
		     relatum (alist-get 'key (alist-get 'outwardIssue entry))))
	      (t nil))
	(when (and relation relatum)
	  (setq result (bd-jira-issue--update-map result relation relatum #'cons)))
	(setq idx (+ idx 1))))
    result))

(defun bd-jira-issue--parse (response-alist)
  "Parse a RESPONSE-ALIST representation of an issue into a plist."
  (let* ((key (alist-get 'key response-alist ""))
	 (fields (alist-get 'fields response-alist ""))
	 (parent (alist-get 'key (alist-get 'parent fields) ""))
	 (assignee (alist-get 'displayName (alist-get 'assignee fields) ""))
	 (reporter (alist-get 'displayName (alist-get 'reporter fields) ""))
	 (issue-type (alist-get 'name (alist-get 'issuetype fields) ""))
	 (summary (alist-get 'summary fields ""))
	 (description (alist-get 'description fields ""))
	 (priority (alist-get 'name (alist-get 'priority fields) ""))
	 (created (alist-get 'created fields ""))
	 (comments (bd-jira-issue--parse-comments fields))
	 (related-issues (bd-jira-issue--parse-relations fields))
	 (status (alist-get 'name (alist-get 'status fields) ""))
	 (result (list :key key
		       :parent parent
		       :assignee assignee
		       :reporter reporter
		       :type issue-type
		       :summary summary
		       :description description
		       :comments comments
		       :priority priority
		       :created created
		       :related related-issues
		       :status status)))
    (dolist (binding *bd-jira-issue/custom-fields*)
      (let* ((jira-value (alist-get (car binding) fields))
	    (name (plist-get (cdr binding) :name))
	    (value (funcall (plist-get (cdr binding) :parse-fn) jira-value)))
	(push value result)
	(push name result)))
    result))

(defun bd-jira-issue--parse-issue-response (response)
  "Turn RESPONSE into a list of plists."
  (let* ((issues (alist-get 'issues response))
	 (result '())
	 (idx-cap (length issues))
	 (idx 0))
    (while (< idx idx-cap)
      (push (bd-jira-issue--parse (aref issues idx)) result)
      (setq idx (+ idx 1)))
    result))

(defun bd-jira-issue/get-issues-from-query (jql-query)
  "Get JIRA issues satisfying JQL-QUERY."
  (bd-jira-issue--parse-issue-response
   (bd-jira-request
    "search"
    :headers '(("Accept" . "application/json")
	       ("Content-Type" . "application/json"))
    :parameters (list (cons "jql" jql-query)
		      (cons 'maxResults 500)
		      (cons 'fields "*all")))))

(defun bd-jira-issue/get-issues-by-reporter (reporter)
  "Get JIRA issues reported by REPORTER."
  (bd-jira-issue/get-issues-from-query
   (format "reporter = %s" (bd-jira-user/id reporter))))

(defun benedict-jira-issue/list (&optional query)
  "Show current list of issues in a buffer with optional jql QUERY."
  (let ((jql (or query "")))
    (sort (bd-jira-issue/get-issues-from-query jql) #'bd-status<=)))

(defun benedict-jira-issue/detail (issue-key)
  "Display the details of ISSUE-KEY."
  (bd-jira-issue--parse
   (bd-jira-request
    (format "issue/%s" issue-key)
    :headers  '(("Accept" . "application/json")
		("Content-Type" . "application/json")))))

;;;;;;;;;;
;;; Create

(defun bd-jira-issue--parse-create-buffer-line (line)
  "Parse the key and value from LINE returning value."
  (string-trim (string-join (cdr (split-string line ":")) ":")))

(defun bd-jira-issue--parse-create-buffer ()
  "Parse a buffer for creating a new jira issue."
  (if (equal (buffer-name (current-buffer)) *benedict-jira-create-issue-buffer*)
      (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
	     (lines (split-string contents "\n"))
	     (title (bd-jira-issue--parse-create-buffer-line (car lines)))
	     (issue-type (bd-jira-issue--parse-create-buffer-line (cadr lines)))
	     (label (bd-jira-issue--parse-create-buffer-line (caddr lines)))
	     (parent (bd-jira-issue--parse-create-buffer-line (cadddr lines)))
	     (description (bd-jira-issue--parse-create-buffer-line
			   (string-join (cddddr lines) "\n")))
	     (tmp-file (make-temp-file "benedict-issue-create")))
	(save-window-excursion
	  (let ((buffer (find-file tmp-file)))
	    (insert contents)
	    (save-buffer)
	    (kill-buffer buffer)))
	(message (format "issue content saved to: %s" tmp-file))
	(list :title title
	      :issue-type issue-type
	      :label label
	      :parent parent
	      :description description))
    (warn (format "Cannot parse create issue from buffer other than %s"
		  *benedict-jira-create-issue-buffer*))))

(defun bd-jira-issue--create-issue ()
  "Create a jira issue from the current buffer."
  (interactive)
  (let ((result nil))
    (when-let ((issue-spec (bd-jira-issue--parse-create-buffer)))
      ;; Create the issue
      (cl-destructuring-bind (&key title issue-type label parent description &allow-other-keys)
	  issue-spec
	(let ((fields (list
		       '(project . ((key . "XDR")))
		       '(components . (((name . "Engine"))))
		       (cons 'description description)
		       (cons 'summary title)
		       ;; when we get multiple do a split on some delimiter char
		       (cons 'labels (list label))
		       (cons 'issuetype (list (cons 'name issue-type))))))
	  (unless (equal (string-trim parent) "")
	    (push (cons 'parent (list (cons 'key parent))) fields))
	  (setq result
		(bd-jira-request
		 "issue"
		 :type "POST"
		 :headers '(("Content-Type" . "application/json")
			    ("Accept" . "application/json"))
		 :data (json-encode (list (cons 'fields fields)))))
	  (message (format "created issue: %s" (alist-get 'key result))))))
    ;; Kill the buffer
    (kill-buffer nil)))

(define-minor-mode benedict-jira-issue--create-mode
    "Minor mode for cerat github items."
  :init-value nil
  :keymap
  (list (cons (kbd "C-c C-c") #'bd-jira-issue--create-issue)))

(defconst *benedict-jira-create-issue-buffer* "*benedict-create-jira-issue*")

(defun bd-jira-issue--issue-types ()
  "Fetch issue types associated with XDR."
  ;; todo: make a project variable
  ;; todo: cache this?
  (thread-last
    (bd-jira-request
     "issue/createmeta/XDR/issuetypes"
     :type "GET"
     :headers
     '(("Content-Type" . "application/json")
       ("Accept" . "application/json")))
    (alist-get 'issueTypes)
    (mapcar
     (lambda (it) (alist-get 'name it)))))

(defun benedict-jira-issue/create ()
  "Generate a buffer to create a new jira issue."
  (interactive)
  (let ((issue-type (completing-read
		     "issue type: " (bd-jira-issue--issue-types)))
	(buffer (switch-to-buffer *benedict-jira-create-issue-buffer*)))
    (kill-region (point-min) (point-max))
    (insert "Title: \n")
    (insert (format "Issue Type: %s\n" issue-type))
    (insert "Label: \n")
    (insert "Parent: \n")
    (insert "Description: \n\n")
    (cond
      ((equal issue-type "Bug")
       (insert
	(string-join
	 (list "Steps to reproduce the behavior: "
	       "Expected behavior: "
	       "Screenshots: "
	       (string-join
		(list
		 "Browser (please complete the following information):"
		 " - OS: [e.g. Windows]: "
		 " - Browser [e.g. firefox, chrome, safari]: "
		 " -  Version [e.g. 22]: ")
		"\n")
	       "Additional Context: ")
	 "\n\n")))
      ((equal issue-type "Task")
       (insert "\n"))
      (t nil))
    (goto-char 0)
    (markdown-mode)
    (benedict-jira-issue--create-mode 1)))

;;;;;;;;
;;; Edit

(defun bd-jira-issue/update-status (issue-key status)
  "Update ISSUE-KEY to have STATUS."
  (if (member status (mapcar #'car *bd-jira-issue/transitions*))
      (let* ((id (alist-get status *bd-jira-issue/transitions* nil nil #'equal))
	     (payload (list
		       (cons 'transition
			     (list (cons 'name status)
				   (cons 'id id))))))
	(bd-jira-request (format "issue/%s/transitions" issue-key)
			 :type "POST"
			 :headers '(("Content-Type" . "application/json")
				    ("Accept" . "application/json"))
			 :data (json-encode payload)))
    (progn
      (warn (format "Cannot update issue: %s is not a known status" status))
      '((success . nil)))))

(defun bd-jira-issue/assign (issue-key &optional account-id)
  "Assign issue with ISSUE-KEY to ACCOUNT-ID or the current user."
  (bd-jira-user/set-user)
  (let ((account (or account-id (bd-jira-config/get :account-id))))
    (when account
      (bd-jira-request (format "issue/%s/assignee" issue-key)
		       :type "PUT"
		       :headers '(("Content-Type" . "application/json")
				  ("Accept" . "application/json"))
		       :data (json-encode (list (cons 'accountId account)))))))

(defun bd-jira-issue/add-label (issue-key label)
  "Add LABEL to issue with ISSUE-KEY."
  (bd-jira-request
   (format "issue/%s" issue-key)
   :type "PUT"
   :headers '(("Content-Type" . "application/json")
	      ("Accept" . "application/json"))
   :data (json-encode
	  (list
	   (cons 'update
		 (list
		  (cons 'labels
			(list (list (cons 'add label))))))))))

(defun bd-jira-issue/add-link (from-key relation to-key)
  "Link FROM-KEY to TO-KEY via RELATION."
  (if (member relation *bd-jira-issue/link-types*)
      (bd-jira-request
       "issueLink"
       :type "POST"
       :headers '(("Content-Type" . "application/json")
		  ("Accept" . "application/json"))
       :data (json-encode (list (cons 'inwardIssue
				      (list (cons 'key from-key)))
				(cons 'outwardIssue
				      (list (cons 'key to-key)))
				(cons 'type
				      (list (cons 'name relation))))))
    (message (format "%s is not a valid link type" relation))))

(defun bd-jira-issue/update-description (issue-key description)
  "Update the description field of ISSUE-KEY to DESCRIPTION."
  (bd-jira-request
   (format "issue/%s" issue-key)
   :type "PUT"
   :headers '(("Content-Type" . "application/json")
	      ("Accept" . "application/json"))
   :data (json-encode (list (cons "fields"
				  (list
				   (cons "description" description)))))))


(defun bd-jira-issue/add-comment (issue-key comment)
  "Add COMMENT to issue with ISSUE-KEY."
  (bd-jira-request
   (format "issue/%s/comment" issue-key)
   :type "POST"
   :headers '(("Content-Type" . "application/json")
	      ("Accept" . "application/json"))
   :data (json-encode (list (cons 'body comment)))))

(defun bd-jira-issue/add-parent (issue-key parent-key)
  "Add PARENT-KEY as a parent to issue with ISSUE-KEY."
  (bd-jira-request
   (format "issue/%s" issue-key)
   :type "PUT"
   :headers '(("Content-Type" . "application/json")
	      ("Accept" . "application/json"))
   :data
   (json-encode
    (list (cons 'fields
		(list (cons 'parent
			    (list (cons 'key parent-key)))))))))

(defun bd-jira-issue/remove-parent (issue-key)
  "Remove PARENT-KEY as a parent from issue with ISSUE-KEY."
  (bd-jira-issue/add-parent issue-key nil))

(defun bd-jira-issue/add-related (

(defun bd-jira-issue/add-sprint (issue-key sprint-name))

(provide 'bd-jira-issue)
;;; bd-jira-issue.el ends here
