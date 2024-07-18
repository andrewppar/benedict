;;; bd-jira-config.el --- Manage jira within Emacs -*- lexical-binding: t -*-

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

;; utils

(defun bd-jira-config--slurp (filepath)
  "Read the contents of FILEPATH as a string."
  (when (file-exists-p filepath)
    (let ((results nil))
      (save-window-excursion
	(let ((buffer (find-file filepath)))
	  (setq results
		(buffer-substring-no-properties (point-min) (point-max)))
	  (kill-buffer buffer)))
      results)))

(defun bd-jira-config--keywordize (string)
  "Turn STRING into a keyword."
  (read (format ":%s" (downcase string))))

(defun bd-jira-config--parse-netrc (netrc-string)
  "Parse NETRC-STRING into a list of plists."
  (let ((lines (split-string netrc-string "\n" t))
	(entries '()))
    (dolist (line lines)
      (let ((fields (split-string line " " t))
	    (entry '()))
	(cl-do ((todo fields (cddr todo))
		(key (car fields) (caddr todo))
		(value (cadr fields) (cadddr todo)))
	       ((not todo) entry)
	  (setq entry (plist-put entry (bd-jira-config--keywordize key) value)))
	(push entry entries)))
    entries))

(defvar *bd-jira-config/netrc* "")

(defvar *bd-jira-config/config* '())

(defun bd-jira-config/init ()
  "Initialize *bd-jira-config/config*."
  (setq *bd-jira-config/config* '())
  (cl-destructuring-bind (&key machine login password &allow-other-keys)
      (cl-some
       (lambda (entry)
	 (when (string-match
		".*\\.atlassian.net$" (plist-get entry :machine))
	   entry))
       (bd-jira-config--parse-netrc
	(bd-jira-config--slurp
	 *bd-jira-config/netrc*)))
    (setq *bd-jira-config/config*
	  (plist-put *bd-jira-config/config* :domain machine))
    (setq *bd-jira-config/config*
	  (plist-put *bd-jira-config/config* :user login))
    (setq *bd-jira-config/config*
	  (plist-put *bd-jira-config/config* :token password))))

(defun bd-jira-config/get (key)
  "Get property stored at KEY in configuration."
  (plist-get *bd-jira-config/config* key))

(defun bd-jira-config/set (key value)
  "Set the property at KEY to VALUE in configuration."
  (setq *bd-jira-config/config*
	(plist-put *bd-jira-config/config* key value)))


(provide 'bd-jira-config)
;;; bd-jira-config.el ends here
