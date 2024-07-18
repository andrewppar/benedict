;;; benedict-config.el --- Manage External Tasks -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 23 May 2024
;; Homepage: N/A
;; Keywords: jira, atlassian, github, git
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 0.0.1

;;; Commentary:

;; The configurations for benedict services are spread across several files
;; this code pulls them together
;;

;;; Code:
(require 'bd-jira-config)
(require 'bd-jira-user)

(defun benedict-config/init! ()
  "Initialize all of benedict's configuration."
  (unless *bd-jira-config/config*
    (bd-jira-config/init)
    (bd-jira-user/set-user)))

(provide 'benedict-config)
;;; benedict-config.el ends here
