;;; bd-jira-org.el --- Integrate JIRA and org -*- lexical-binding: t -*-

;; Copyright (C) 2024 Andrew Parisi
;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 30 October 2024
;; Homepage: N/A
;; Keywords: jira, org, benedict
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 0.1

;;; Commentary:

;; Provide utilities for interacting with jira in org mode
;;
;; Benedict provides an interface for handling jira, org provides
;; an interface for everything else.  So it's good if we can connect
;; the two.

;;; API:

;; Customizable Variables
;;  bd-jira-org/priorities
;;  bd-jira-org/jira-file
;;  bd-jira-org/keywords
;;
;; Functions:
;; bd-jira-org/->org
;; bd-jira-org/add-issue
;; bd-jira-org/refresh

;;; Code:
(require 'org)
(require 'bd-jira-config)
(require 'bd-jira-issue)


;;; From JIRA
(defvar bd-jira-org/board-ids '()
  "A list of integers representing boards that are relevant.
   To find the relevant list of ids call `bd-jira-board/list`.")

(defvar bd-jira-org/priorities '()
  "An alist that maps organizational JIRA priorities to org priorities.")

(defvar bd-jira-org/jira-file nil
  "The file that holds jira information.")

(defun bd-jira-org--priority->org-priority (priority)
  "Convert jira PRIORITY to an org priority level."
  (plist-get bd-jira-org/priorities priority #'equal))

(defun bd-jira-org--in-progress-keywords ()
  "Get org keywords that represent that something is not completed."
  (let ((result '()))
    (dolist (keywords (reverse org-todo-keywords))
      (let* ((without-tags (cdr keywords))
	     (in-progress (if (member "|" without-tags)
			      (seq-take-while
			       (lambda (keyword) (not (equal keyword "|")))
			       without-tags)
			    (butlast without-tags))))
	(dolist (key (reverse in-progress))
	  (push key result))))
    result))

(defun bd-jira-org--not-done-p (status)
  "Check if STATUS is an in progress status."
  (member status (bd-jira-org--in-progress-keywords)))

(defun bd-jira-org--make-stars (number)
  "Make a string of NUMBER many stars."
  (make-string number ?\*))

(defun bd-jira-org--split-subtasks (subtasks subtask-depth)
  "INTERNAL: This function is only for implementation.

It splits SUBTASKS on SUBTASK-DEPTH number of *s, when SUBTASKS is not NIL."
  (when subtasks
    (let ((split-regex (regexp-quote (bd-jira-org--make-stars subtask-depth))))
      (mapcar #'string-trim (string-split subtasks split-regex t)))))

(defvar bd-jira-org--subtask-depth 4)

(defvar bd-jira-org/keywords (list :default "DONE")
  "An alist of JIRA status keywords mapped to org todo statuses.
A :default key should be specified - it is set to DONE otherwise.")

(defun bd-jira-org--derived-in-progress-todo-key (subtasks)
  "Return the todo key for SUBTASKS if any is in progress."
  (seq-some
   (lambda (subtask)
     (seq-some
      (lambda (keyword)
	(when (string-prefix-p keyword subtask)
	  keyword))
      (bd-jira-org--in-progress-keywords)))
   (bd-jira-org--split-subtasks subtasks bd-jira-org--subtask-depth)))

(defun bd-jira-org--status->todo-key (status subtasks)
  "Determine the todo key for jira STATUS with SUBTASKS."
  (or (bd-jira-org--derived-in-progress-todo-key subtasks)
      (plist-get bd-jira-org/keywords status #'equal)
      (plist-get bd-jira-org/keywords :default)
      "DONE"))

(defun bd-jira-org--quote (string)
  "Ensure that STRING is insertable in rog mode."
  (if string
      (string-join
       (mapcar (lambda (line) (format " %s" line)) (split-string string "\n"))
       "\n")
    ""))

(defun bd-jira-org--key-at-point ()
  "Get the key for the issue at point."
  (org-entry-get (point) "ID" 'select))

(defun bd-jira-org--key->subtasks ()
  "Generate alist of jira issue key to subtasks from current buffer."
  (let ((result '()))
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
	 (when (equal (org-get-heading t t t t) "subtasks")
	   (let ((key (org-entry-get nil "ID" 'select))
		 (subtasks (org-get-entry)))
	     (push (cons key subtasks) result))))))
    result))

(defun bd-jira-org--subtask-get (key->subtasks key)
  "Get subtasks for KEY in KEY->SUBTASKS alist."
  (alist-get key key->subtasks nil nil #'equal))

(defun bd-jira-org--jira-md->org (string)
  "Use pandoc to convert STRING to org mode if available."
  ;; maybe cache pandoc exetule check
  (let ((executable (string-trim (shell-command-to-string "which pandoc"))))
    (if (equal executable "")
	(bd-jira-org--quote string)
      (bd-jira-org--quote
       (shell-command-to-string
	(format "echo \"%s\" | %s -f jira -t org" string executable))))))

(defun bd-jira-org--serialize-comments (stars comments)
  "Serialize COMMENTS to org mode with prefix STARS."
  (string-join
   (mapcar
    (lambda (jira-comment)
      (cl-destructuring-bind (&key comment author created &allow-other-keys)
	  jira-comment
	(string-join
	 (list
	  (format "%s comment:" stars)
	  (bd-jira-org--quote comment)
	  ":PROPERTIES:"
	  (format ":author: %s" author)
	  (format ":created: %s" created)
	  ":END:")
	 "\n")))
    comments)
   "\n"))

(defun bd-jira-org--key->link (issue-key)
  "Create a link to browse issue from ISSUE-KEY."
  (format "https://%s/browse/%s"
	  (plist-get *bd-jira-config/config* :domain)
	  issue-key))

(defun bd-jira-org/->org (issue key->subtasks &optional depth keep-complete?)
  "Format ISSUE as an org mode string.
Use KEY->SUBTASKS to determine some of it's state.
Optionally specify the DEPTH to make the issue.
Optionally specify KEEP-COMPLETE? to retian items that are no longer
in TODO or PROG states."
  (let* ((depth (or depth 1))
	 (key-stars (bd-jira-org--make-stars depth))
	 (main-stars (bd-jira-org--make-stars (+ 1 depth)))
	 (task-stars (bd-jira-org--make-stars (+ 2 depth))))
    (cl-destructuring-bind
	  (&key assignee created key description parent priority reporter
		status comments summary sprints related  type &allow-other-keys)
	issue
      (let* ((org-priority (bd-jira-org--priority->org-priority priority))
	     (link (format "[[%s][%s]]" (bd-jira-org--key->link key) key))
	     (tag (format ":%s:" (string-replace "-" "_" key)))
	     (org-status (bd-jira-org--status->todo-key status (bd-jira-org--subtask-get key->subtasks key))))
	(when (equal type "Epic")
	  (setq tag (format "%sepic:" tag)))
	(when (or (and (not keep-complete?) (bd-jira-org--not-done-p org-status))
		  keep-complete?)
	  (let ((main-task (format "%s %s [#%s] %s: %s %s"
				   key-stars org-status org-priority link summary tag))
		(org-desc (format "%s description:\n%s"
				  main-stars (bd-jira-org--jira-md->org description)))
		(org-comments (bd-jira-org--serialize-comments main-stars comments))
		(properties (list (format ":assignee: %s" assignee)
				  (format ":status: %s" status)
				  (format ":reporter: %s" reporter)
				  (format ":type: %s" type)
				  (format ":parent: %s" parent)
				  (format ":priority: %s" priority)
				  (format ":title: %s" summary)
				  (format ":created: %s" created)
				  (format ":ID: %s" key)
				  ":END:")))
	    (when sprints
	      (dolist (sprint sprints)
		(push (format ":sprint: %s" (plist-get sprint :name)) properties)))
	    (push ":PROPERTIES:" properties)
	    (string-join
	     (seq-concatenate 'list (list main-task) properties (list org-desc org-comments "\n"))
	     "\n")))))))

(defun bd-jira-org/add-issue (issue-key)
  "Add issue with ISSUE-KEY to org file.
If it exists remove it so it can be refreshed."
  (save-excursion
    (when-let ((pos (org-id-find-id-in-file issue-key bd-jira-org/jira-file 'marker)))
      (goto-char pos)
      (kill-region pos (org-end-of-subtree t)))
    (goto-char (point-max))
    (when-let ((issue (bd-jira-org/->org (benedict-jira-issue/detail issue-key) nil 2 t)))
      (insert issue))))

(defun bd-jira-org--insert-subtasks (key->task)
  "Insert subtasks for any issues in KEY->TASK."
  (dolist (key-with-saved-tasks key->task)
    (let* ((key (car key-with-saved-tasks))
	   (pos (org-id-find-id-in-file key bd-jira-org/jira-file 'marker)))
      (unless pos
	(bd-jira-org/add-issue key)
	(setq pos (org-id-find-id-in-file key bd-jira-org/jira-file 'marker)))
      (goto-char pos)
      (org-goto-first-child)
      (let ((depth (org-current-level)))
	(org-back-to-heading)
	(org-insert-heading nil t depth)
	(org-move-subtree-down)
	(org-end-of-line 1)
	(insert "subtasks\n")
	(let ((subtasks (cdr key-with-saved-tasks)))
	  (when (and subtasks (not (equal (string-trim subtasks) "")))
	    (insert (format "%s" subtasks))))))))

(defun bd-jira-org--build-refresh-query (project component)
  "Build a query for JIRA to fetch issues for PROJECT and COMPONENT."
  (let ((clauses '()))
    (push "status != Done" clauses)
    (push (format "project = %s" project) clauses)
    (when component
      (push (format "component = %s" component) clauses))
    (string-join clauses " and ")))

;; TODO: generalize this to handle multiple projects
(defun bd-jira-org/refresh (project &optional component)
  "Refresh the file that has JIRA issues using PROJECT.
Optionally also specify a JIRA COMPONENT."
  (benedict/init!)
  (save-excursion
    (goto-char (point-min))
    (let ((key->task (bd-jira-org--key->subtasks)))
      (kill-region (point-min) (point-max))
      (insert "#+STARTUP: show2levels\n")
      (insert (format "* %s\n" project))
      (insert ":PROPERTIES:\n")
      (insert (format ":CATEGORY: %s\n" project))
      (insert ":TIMELINE_FACE: \"#225E8B\"\n")
      (insert ":END:\n")
      (let* ((query (bd-jira-org--build-refresh-query project component))
	     (issues (bd-jira-issue/get-issues-from-query query)))
	(goto-char (point-max))
	(insert "\n")
	(dolist (issue issues)
	  (when-let ((issue-text (bd-jira-org/->org issue key->task 2)))
	    (insert issue-text))))
      (bd-jira-org--insert-subtasks key->task))))

;;; To JIJRA

;; comment
(defconst bd-jira-org--input-buffer "*benedict-jira-input*")
(defvar bd-jira-org--input-data '())
(defvar bd-jira-org--saved-layout ())

(define-minor-mode bd-jira-org/input-mode
    "Minor mode to get input in benedict jira."
  :init-value nil
  :lighter " benedict"
  :keymap `((,(kbd "q") . kill-buffer)
	    (,(kbd "C-c C-c") . bd-jira-org/send-input-buffer)))

(defun bd-jira-org/send-input-buffer ()
  "Call the add comment function with the appropriate args from buffer."
  (interactive)
  (let* ((buffer (switch-to-buffer bd-jira-org--input-buffer))
	 (to-send (buffer-substring-no-properties (point-min) (point-max)))
	 (key (alist-get 'key bd-jira-org--input-data)))
    (bd-jira-issue/add-comment key to-send)
    (set-window-configuration bd-jira-org--saved-layout)
    (setq bd-jira-org--saved-layout nil
	  bd-jira-org--input-data nil)
    (kill-buffer buffer)))

(defun bd-jira-org--get-input ()
  "Generate a new buffer to get input."
  (setq bd-jira-org--saved-layout (current-window-configuration))
  (select-window (split-window-below nil (frame-root-window)))
  (switch-to-buffer bd-jira-org--input-buffer)
  (kill-region (point-min) (point-max))
  (bd-jira-org/input-mode 1))

(defun bd-jira-org/add-comment ()
  "Add a comment to the issue at point."
  (interactive)
  (let ((key (bd-jira-org--key-at-point)))
    (push (cons 'key key) bd-jira-org--input-data)
    (bd-jira-org--get-input)))

(defun bd-jira-org/current-buffer-as-comment (issue-key)
  "Add current buffer as a comment to ISSUE-KEY."
  (interactive (list (read-string "issue: ")))
  (let ((comment (buffer-substring-no-properties (point-min) (point-max))))
    (bd-jira-issue/add-comment issue-key comment)))

;; status

(defun bd-jira-org/change-status (status)
  "Update the status of the issue at point to STATUS."
  (interactive
   (list
    (completing-read "new status: "
		     (mapcar #'car *bd-jira-issue/transitions*)
		     nil
		     t)))
  (let ((key (bd-jira-org--key-at-point)))
    (benedict/issue-update key :status status)))

;; assign

(defun bd-jira-org/assign ()
  "Assign issue at point to current user."
  (interactive)
  (let ((key (bd-jira-org--key-at-point)))
    (benedict/issue-update key :assign)))

;; link

(defun bd-jira-org/link ()
  "Link issue at point to other issues."
  (interactive)
  (let ((key (bd-jira-org--key-at-point))
	(relation (completing-read "relation: " (bd-jira-issue/link-types)))
	(linked-issue (read-string "linked issue key: ")))
    (benedict/issue-update key :link relation linked-issue)))

;; sprint

(defun bd-jira-org/add-issue-to-sprint ()
  "Add issue at point to a sprint."
  (interactive)
  (benedict/init!)
  (let* ((key (bd-jira-org--key-at-point))
	 (sprints (bd-jira-board/sprints bd-jira-org/board-ids))
	 (selected-sprint (completing-read
			  "sprint: "
			  (mapcar
			   (lambda (sprint) (plist-get sprint :name))
			   sprints)
			  nil t))
	 (sprint-id (seq-some
		     (lambda (sprint)
		       (when (equal (plist-get sprint :name) selected-sprint)
			 (plist-get sprint :id)))
		     sprints)))
    (benedict/issue-update key :sprint sprint-id)))

(provide 'bd-jira-org)
;;; bd-jira-org.el ends here
