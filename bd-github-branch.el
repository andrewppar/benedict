;;; bd-github-branch.el --- Manage github within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Andrew Parisi
;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 14 April 2023
;; Homepage: N/A
;; Keywords: git, github
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 3.0

;;; Commentary:

;; Provide utilities for interacting with the github command line tool


;; Wrap the github CLI with Emacs, so you can keep on using magit.
;; This is a little more targeted and lightweight than forge.
;; Auth is handled by the gh executable itself and only works for github.


;;; Code:
(require 'vc)
(require 'magit-branch)
(require 'bd-jira-issue)
(require 'bd-github-utils)

(defun bd-github-branch--clean-issue-title (title)
  "Create a magit branch acceptable title from TITLE."
  (thread-last
    title
    split-string
    (mapcar #'downcase)
    ((lambda (strings) (string-join strings "-")))
    (replace-regexp-in-string "," "")
    (replace-regexp-in-string "\\." "")
    (replace-regexp-in-string "\"" "")
    (replace-regexp-in-string ":" "")
    (replace-regexp-in-string "`" "")
    (replace-regexp-in-string "_" "")
    (replace-regexp-in-string "/" "")))

(defun bd-github-branch--spinoff-branch-from-jira-issue (issue-key)
  "Create branch for JIRA ISSUE-KEY."
  (cl-destructuring-bind (&key summary &allow-other-keys)
      ;; TODO: roll github stuff into benedict and allow it to
      ;; chose the issue type to look for either github or JIRA
      (benedict-jira-issue/detail issue-key)
    (thread-last summary
		 bd-github-branch--clean-issue-title
		 (format "%s/%s" issue-key)
		 magit-branch-spinoff)))

;; Maybe this belongs at a level up?
;; as GIT stuff develops here, we can move it up.
(defun bd-github-branch--set-git-dir (directory)
  "Return DIRECTORY if it is under git control, otherwise prompt for one that is."
  (cl-flet ((git-dir-p (dir) (equal 'Git
				    (condition-case nil
					(vc-responsible-backend dir)
				      (error nil)))))
    (if (git-dir-p directory)
	directory
      (progn
	(message (format "%s is not a git controlled directory." directory))
	(let ((new-directory (read-directory-name "Select a git directory: ")))
	  (if (git-dir-p new-directory)
	      new-directory
	    (progn
	      (warn (format "%s is not a git controlled directory" new-directory))
	      nil)))))))

(defun bd-github-branch/spinoff-from-key (issue-key)
  "Create brnach from ISSUE-KEY.
Potentially asking to set git directory."
  (let ((jira-issue? t)
	(project-dir (bd-github-branch--set-git-dir default-directory)))
    (when project-dir
      (let ((default-directory project-dir))
	(cond (jira-issue?
	       (bd-github-branch--spinoff-branch-from-jira-issue issue-key))
	      (t nil))))))


(provide 'bd-github-branch)
;;; bd-github-branch.el ends here
