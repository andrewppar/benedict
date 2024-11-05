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
;;  bdjo/priorities
;;  bdjo/jira-file
;;  bdjo/keywords
;;
;; Functions:
;; bdjo/->org
;; bdjo/add-issue
;; bdjo/refresh

;;; Code:
(require 'org)
(require 'bd-jira-config)
(require 'bd-jira-issue)


;;; From JIRA

(defvar bdjo/priorities '()
  "An alist that maps organizational JIRA priorities to org priorities.")

(defvar bdjo/jira-file nil
  "The file that holds jira information.")

(defun bdjo-priority->org-priority (priority)
  "Convert jira PRIORITY to an org priority level."
  (plist-get bdjo/priorities priority #'equal))

(defun bdjo-in-progress-keywords ()
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

(defun bdjo-not-done-p (status)
  "Check if STATUS is an in progress status."
  (member status (bdjo-in-progress-keywords)))

(defun bdjo-make-stars (number)
  "Make a string of NUMBER many stars."
  (make-string number ?\*))

(defun bdjo-split-subtasks (subtasks subtask-depth)
  "INTERNAL: This function is only for implementation.

It splits SUBTASKS on SUBTASK-DEPTH number of *s, when SUBTASKS is not NIL."
  (when subtasks
    (let ((split-regex (regexp-quote (bdjo-make-stars subtask-depth))))
      (mapcar #'string-trim (string-split subtasks split-regex t)))))

(defvar bdjo-subtask-depth 4)

(defvar bdjo/keywords (list :default "DONE")
  "An alist of JIRA status keywords mapped to org todo statuses.
A :default key should be specified - it is set to DONE otherwise.")

(defun bdjo-derived-in-progress-todo-key (subtasks)
  "Return the todo key for SUBTASKS if any is in progress."
  (seq-some
   (lambda (subtask)
     (seq-some
      (lambda (keyword)
	(when (string-prefix-p keyword subtask)
	  keyword))
      (bdjo-in-progress-keywords)))
   (bdjo-split-subtasks subtasks bdjo-subtask-depth)))

(defun bdjo-status->todo-key (status subtasks)
  "Determine the todo key for jira STATUS with SUBTASKS."
  (or (bdjo-derived-in-progress-todo-key subtasks)
      (plist-get bdjo/keywords status #'equal)
      (plist-get bdjo/keywords :default)
      "DONE"))

(defun bdjo-quote (string)
  "Ensure that STRING is insertable in rog mode."
  (if string
      (string-join
       (mapcar (lambda (line) (format " %s" line)) (split-string string "\n"))
       "\n")
    ""))

(defun bdjo-key->subtasks ()
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

(defun bdjo-subtask-get (key->subtasks key)
  "Get subtasks for KEY in KEY->SUBTASKS alist."
  (alist-get key key->subtasks nil nil #'equal))

(defun bdjo-jira-md->org (string)
  "Use pandoc to convert STRING to org mode if available."
  ;; maybe cache pandoc exetule check
  (let ((executable (string-trim (shell-command-to-string "which pandoc"))))
    (if (equal executable "")
	(bdjo-quote string)
      (bdjo-quote
       (shell-command-to-string
	(format "echo \"%s\" | %s -f jira -t org" string executable))))))

(defun bdjo-serialize-comments (stars comments)
  "Serialize COMMENTS to org mode with prefix STARS."
  (string-join
   (mapcar
    (lambda (jira-comment)
      (cl-destructuring-bind (&key comment author created &allow-other-keys)
	  jira-comment
	(string-join
	 (list
	  (format "%s comment:" stars)
	  (bdjo-quote comment)
	  ":PROPERTIES:"
	  (format ":author: %s" author)
	  (format ":created: %s" created)
	  ":END:")
	 "\n")))
    comments)
   "\n"))

(defun bdjo-key->link (issue-key)
  "Create a link to browse issue from ISSUE-KEY."
  (format "https://%s/browse/%s"
	  (plist-get *bd-jira-config/config* :domain)
	  issue-key))

(defun bdjo/->org (issue key->subtasks &optional depth keep-complete?)
  "Format ISSUE as an org mode string.
Use KEY->SUBTASKS to determine some of it's state.
Optionally specify the DEPTH to make the issue.
Optionally specify KEEP-COMPLETE? to retian items that are no longer
in TODO or PROG states."
  (let ((depth (or depth 1))
	(key-stars (bdjo-make-stars depth))
	(main-stars (bdjo-make-stars (+ 1 depth)))
	(task-stars (bdjo-make-stars (+ 2 depth))))
    (cl-destructuring-bind
	  (&key assignee created key description parent priority reporter
		status comments summary related type &allow-other-keys)
	issue
      (let* ((org-priority (bdjo-priority->org-priority priority))
	     (link (format "[[%s][%s]]" (bdjo-key->link key) key))
	     (tag (format ":%s:" (string-replace "-" "_" key)))
	     (org-status (bdjo-status->todo-key status (bdjo-subtask-get key->subtasks key))))
	(when (equal type "Epic")
	  (setq tag (format "%sepic:" tag)))
	(when (or (and (not keep-complete?) (bdjo-not-done-p org-status))
		  keep-complete?)
	  (let ((main-task (format "%s %s [#%s] %s: %s %s"
				   key-stars org-status org-priority link summary tag))
		(org-desc (format "%s description:\n%s"
				  main-stars (bdjo-jira-md->org description)))
		(org-comments (bdjo-serialize-comments main-stars comments)))
	    (string-join
	     (list main-task
		   ":PROPERTIES:"
		   (format ":assignee: %s" assignee)
		   (format ":status: %s" status)
		   (format ":reporter: %s" reporter)
		   (format ":type: %s" type)
		   (format ":parent: %s" parent)
		   (format ":priority: %s" priority)
		   (format ":title: %s" summary)
		   (format ":created: %s" created)
		   (format ":ID: %s" key)
		   ":END:"
		   org-desc
		   org-comments
		   "\n")
	     "\n")))))))

(defun bdjo/add-issue (issue-key)
  "Add issue with ISSUE-KEY to org file.
If it exists remove it so it can be refreshed."
  (save-excursion
    (when-let ((pos (org-id-find-id-in-file issue-key bdjo/jira-file 'marker)))
	  (goto-char pos)
	  (kill-region pos (org-end-of-subtree t)))
    (goto-char (point-max))
    (when-let ((issue (bdjo/->org (benedict/issue-detail issue-key) nil 2 t)))
      (insert issue))))

(defun bdjo-insert-subtasks (key->task)
  "Insert subtasks for any issues in KEY->TASK."
  (dolist (key-with-saved-tasks key->task)
    (let* ((key (car key-with-saved-tasks))
	   (pos (org-id-find-id-in-file key bdjo/jira-file 'marker)))
      (unless pos
	(bdjo/add-issue key)
	(setq pos (org-id-find-id-in-file key bdjo/jira-file 'marker)))
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

(defun bdjo-build-refresh-query (project component)
  "Build a query for JIRA to fetch issues for PROJECT and COMPONENT."
  (let ((clauses '()))
    (push "status != Done" clauses)
    (push (format "project = %s" project) clauses)
    (when component
      (push (format "component = %s" component) clauses))
    (string-join clauses " and ")))

;; TODO: generalize this to handle multiple projects
(defun bdjo/refresh (project &optional component)
  "Refresh the file that has JIRA issues using PROJECT.
Optionally also specify a JIRA COMPONENT."
  (benedict/init!)
  (save-excursion
    (goto-char (point-min))
    (let ((key->task (bdjo-key->subtasks)))
      (kill-region (point-min) (point-max))
      (insert "#+STARTUP: show2levels\n")
      (insert (format "* %s\n" project))
      (insert ":PROPERTIES:\n")
      (insert (format ":CATEGORY: %s\n" project))
      (insert ":TIMELINE_FACE: \"#225E8B\"\n")
      (insert ":END:\n")
      (let* ((query (bdjo-build-refresh-query project component))
	     (issues (bd-jira-issue/get-issues-from-query query)))
	(goto-char (point-max))
	(insert "\n")
	(dolist (issue issues)
	  (when-let ((issue-text (bdjo/->org issue key->task 2)))
	    (insert issue-text))))
      (bdjo-insert-subtasks key->task))))

;;; To JIJRA

;; comment
(defconst bdjo-input-buffer "*benedict-jira-input*")
(defvar bdjo-input-data '())
(defvar bdjo-saved-layout ())

(define-minor-mode bdjo/input-mode
    "Minor mode to get input in benedict jira."
  :init-value nil
  :lighter " benedict"
  :keymap `((,(kbd "q") . kill-buffer)
	    (,(kbd "C-c C-c") . bdjo/send-input-buffer)))

(defun bdjo/send-input-buffer ()
  "Call the add comment function with the appropriate args from buffer."
  (interactive)
  (let* ((buffer (switch-to-buffer bdjo-input-buffer))
	 (to-send (buffer-substring-no-properties (point-min) (point-max)))
	 (key (alist-get 'key bdjo-input-data)))
    (bd-jira-issue/add-comment key to-send)
    (set-window-configuration bdjo-saved-layout)
    (setq bdjo-saved-layout nil
	  bdjo-input-data nil)
    (kill-buffer buffer)))

(defun bdjo-get-input ()
  "Generate a new buffer to get input."
  (setq bdjo-saved-layout (current-window-configuration))
  (select-window (split-window-below nil (frame-root-window)))
  (switch-to-buffer bdjo-input-buffer)
  (kill-region (point-min) (point-max))
  (bdjo/input-mode 1))

(defun bdjo/add-comment ()
  "Add a comment to the issue at point."
  (interactive)
  (let ((key (org-entry-get (point) "ID" 'selection)))
    (push (cons 'key key) bdjo-input-data)
    (bdjo-get-input)))

(defun bdjo/current-buffer-as-comment (issue-key)
  "Add current buffer as a comment to ISSUE-KEY."
  (interactive (list (read-string "issue: ")))
  (let ((comment (buffer-substring-no-properties (point-min) (point-max))))
    (bd-jira-issue/add-comment issue-key comment)))

;; status

(defun bdjo/change-status (status)
  "Update the status of the issue at point to STATUS."
  (interactive
   (list
    (completing-read "new status: "
		     (mapcar #'car *bd-jira-issue/transitions*)
		     nil
		     t)))
  (let ((key (org-entry-get (point) "ID" 'selective)))
    (benedict/issue-update key :status status)))

;; assign

(defun bdjo/assign ()
  "Assign issue at point to current user."
  (interactive)
  (let ((key (org-entry-get (point) "ID" 'selective)))
    (benedict/issue-update key :assign)))


(provide 'bd-jira-org)
;;; bd-jira-org.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("bdjo-" . "bd-jira-org--") ("bdjo/" . "bd-jira-org/"))
;; End:
