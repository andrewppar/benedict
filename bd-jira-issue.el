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

;; View, Create, Edit, and Delete Issues

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
  ;;   (lambda (link-type) (alist-get 'name link-type)
  ;;     ))
  ;;  remove-duplicates)
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
	 (status (alist-get 'name (alist-get 'status fields) "")))
    (list :key key
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
		      (cons "maxResults" "500")
		      (cons 'fields "*all")))))


(defun bd-jira-issue/get-issues-by-reporter (reporter)
  "Get JIRA issues reported by REPORTER."
  (bd-jira-issue/get-issues-from-query
   (format "reporter = %s" (bd-jira-user/id reporter))))

;;;;;;;;;
;;; View

(define-minor-mode bd-jira-issue--view-mode
    "Minor mode for viewing github items."
  :init-value nil)
(evil-define-minor-mode-key 'normal
    'bd-jira-issue--view-mode
  "q"  (lambda () (interactive) (kill-buffer nil)))

(defmacro bd-jira-issue--with-view-buffer (&rest body)
  "Execute BODY within the jira-issue-list buffer."
  `(progn
     (switch-to-buffer "*jira-issue-list*")
     (read-only-mode -1)
     (bd-jira-issue--view-mode 1)
     (kill-region (point-min) (point-max))
     (progn ,@body)
     (read-only-mode 1)))

(defun bd-jira-issue--get-column-max
    (column rows)
  "Get the max char length of COLUMN in ROWS."
  (let ((result (- (length (format "%s" column)) 1)))
    (dolist (row rows)
      (let ((row-result (length (plist-get row column))))
	(when (> row-result result)
	  (setq result row-result))))
    (+ result 1)))

(defun bd-jira-issue--display-table
    (columns table &optional column->transform-fn column->color-fn)
  "Display COLUMNS of TABLE.
Potentially transforming cells with COLUMN->TRANSFORM-FN.
Potentially coloring cells with COLUMN->COLOR-FN."
  (bd-jira-issue--with-view-buffer
   (let ((column->size '())
	 (rows '()))
     (kill-region (point-min) (point-max))
     (dolist (row table)
       (let ((new-row '()))
	 (dolist (column columns)
	   (let* ((cell-fn (alist-get column column->transform-fn #'identity))
		  (cell-content (funcall cell-fn (plist-get row column))))
	     (setq new-row (plist-put new-row column cell-content))))
	 (push new-row rows)))
     (dolist (column columns)
       (let* ((column-max (bd-jira-issue--get-column-max column rows))
	      (column-name (substring (format "%s" column) 1))
	      (padding (- column-max (length column-name))))
	 (insert (format "%s%s" column-name (make-string padding ? )
			 (push (cons column column-max) column->size)))))
     (insert "\n")
     (insert (format "%s\n" (make-string (apply #'+ (mapcar #'cdr column->size)) ?=)))
     (dolist (row rows)
       (dolist (column columns)
	 (let* ((cell-content (plist-get row column))
		(color-fn (alist-get column column->color-fn))
		(color (when color-fn (funcall color-fn cell-content)))
		(padding (- (alist-get column column->size) (length cell-content)))
		(cell-string (format "%s%s" cell-content (make-string padding ? )))
		(cell (if color
			  (propertize cell-string 'face `(:foreground ,color))
			cell-string)))
	   (insert cell)))
       (insert "\n")))))

(defun bd-jira-issue--status-color (status)
  "Return a color for a jira STATUS."
  (cond ((equal status "Selected for Development") "green")
	((equal status "Done") "red")
	((equal status "In Progress") "orange")
	((equal status "Backlog") "grey")
	(t "cyan")))

(defun bd-jira-issue--truncate (string &optional width)
  "Truncate STRING to WIDTH - the default value is 30 chars."
  (let ((max-size (or width 100)))
    (if (> (length string) max-size)
	(format "%s..." (substring string 0 (- max-size 3)))
      string)))

(defun bd-jira-issue--list-position (item list)
  "Find position of ITEM in LIST."
  (let ((idx 0)
	(found? nil)
	(idx-end (length list)))
    (while (and (not found?) (< idx idx-end))
      (when (equal item (nth idx list))
	(setq found? t))
      (when (not found?)
	(setq idx (+ idx 1))))
    (if found? idx -1)))

(defun bd-status<= (issue-one issue-two)
  "Check whether the status of ISSUE-ONE is less than ISSUE-TWO."
  (let ((status-one-idx (bd-jira-issue--list-position
			 (plist-get issue-one :status)
			 *bd-jira-issue/statuses*))
	(status-two-idx (bd-jira-issue--list-position
			 (plist-get issue-two :status)
			 *bd-jira-issue/statuses*)))
    (<= status-one-idx status-two-idx)))

(defun bd-jira-issue/display-issues (issues)
  "Show ISSUES in a dedicated buffer."
  (let ((columns '(:key :status :summary :assignee :reporter)))
    (bd-jira-issue--display-table
     columns issues
     '((:summary . bd-jira-issue--truncate))
     '((:status . bd-jira-issue--status-color)
       (:key . (lambda (x) "yellow"))))))

(defun benedict-jira-issue/list (&optional query)
  "Show current list of issues in a buffer with optional jql QUERY."
  (let* ((jql (or query ""))
	 (issues (sort (bd-jira-issue/get-issues-from-query jql) #'bd-status<=)))
    (bd-jira-issue/display-issues issues)))

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
	     (parent (bd-jira-issue--parse-create-buffer-line (caddr lines)))
	     (description (bd-jira-issue--parse-create-buffer-line
			   (string-join (cdddr lines) "\n"))))
	(list :title title
	      :issue-type issue-type
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
      (cl-destructuring-bind (&key title issue-type parent description &allow-other-keys)
	  issue-spec
	(let ((fields (list
		       '(project . ((key . "XDR")))
		       '(components . (((name . "Engine"))))
		       (cons 'description description)
		       (cons 'summary title)
		       (cons 'issuetype (list (cons 'name issue-type))))))
	  (unless (equal (string-trim parent) "")
	    (push (cons 'parent (list (cons 'key parent))) fields))
	  (setq result
		(bd-jira-request
		 "issue"
		 :type "POST"
		 :headers '(("Content-Type" . "application/json")
			    ("Accept" . "application/json"))
		 :data (json-encode (list (cons 'fields fields))))))))
    ;; Kill the buffer
    (kill-buffer nil)))

(define-minor-mode benedict-jira-issue--create-mode
    "Minor mode for cerat github items."
  :init-value nil)
(evil-define-minor-mode-key 'normal
    'benedict-jira-issue--create-mode
  (kbd "C-c C-c") 'bd-jira-issue--create-issue
  "q" (lambda () (interactive) (kill-buffer nil)))

(defconst *benedict-jira-create-issue-buffer* "*benedict-create-jira-issue*")

(defun benedict-jira-issue/create ()
  "Generate a buffer to create a new jira issue."
  (interactive)
  (let ((buffer (switch-to-buffer *benedict-jira-create-issue-buffer*)))
    (kill-region (point-min) (point-max))
    (insert "Title: \n")
    (insert "Issue Type: Task\n")
    (insert "Parent: \n")
    (insert "Description: \n\n")
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


(provide 'bd-jira-issue)
;;; bd-jira-issue.el ends here
