;;; bd-github-utils.el --- Manage github within Emacs -*- lexical-binding: t -*-

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
(require 'bytecomp)
(byte-compile-disable-warning 'cl-functions)

(defvar *bd-utils/gh-executable*
  (string-trim
   (shell-command-to-string "which gh")))

(defun bd-utils/gh! (object command &rest keylist)
  "Execute a github command for OBJECT with COMMAND.
Optionally can pass an IDENTIFIER for OBJECT and FLAGS to use as keys in
KEYLIST."
  (if *bd-utils/gh-executable*
      (let ((args (list command object)))
	(when-let ((identifier (plist-get keylist :identifier)))
	  (push identifier args))
	(when-let ((flags (plist-get keylist :flags)))
	  (cl-do ((todo (cdr flags) (cdr todo))
		  (key (caar flags) (caar todo))
		  (value (cdar flags) (cdar todo)))
		 ((not todo) (progn (push key args)
				    (unless (equal value "")
				      (push value args))))
	    (progn (push key args)
		   (unless (equal value "")
		     (push value args)))))
	(setq args (reverse args))
	(let ((output-buffer-name "*github-command-buffer*")
    	      (result nil))
	  (save-window-excursion
    	    (let ((output-buffer (switch-to-buffer output-buffer-name)))
    	      (apply #'call-process *bd-utils/gh-executable* nil output-buffer nil args)
    	      (setq result (buffer-substring (point-min) (point-max)))
    	      (kill-buffer output-buffer)))
	  (mapcar #'string-trim (split-string (string-trim result) "\n"))))
    (warn "Attempting to run github cli (gh) command without a gh executable.")))

(defun gh-utils/parse-table-lines
    (table-lines columns &optional column->transform)
  "Make a structured table out of TABLE-LINES and COLUMNS.

TABLE-LINES should be the result of running a github-command that
returns a table representation of its results.

Optionally pass an alist of columns to tranform functions COLUMN->TRANSFORM
that maps columns to a transformation function, e.g. split on ','."
  (let ((result '()))
    (dolist (line table-lines)
      ;; TODO: Check if gh uses something other than \t
      ;; TODO: Abstract out delimiter if it does
      (let ((cells (split-string line "\t"))
	    (idx 0)
	    (row '()))
	(dolist (cell cells)
	  (let* ((column (nth idx columns))
		 (tx-fn (alist-get column column->transform)))
	    (when tx-fn
	      (setq cell (funcall tx-fn cell)))
	    (setq row (plist-put row column cell)
		  idx (+ idx 1))))
	(push (reverse row) result)))
    result))

(provide 'bd-github-utils)
;;; bd-github-utils.el ends here
