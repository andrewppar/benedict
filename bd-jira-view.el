;;; bd-jira-view.el --- Manage jira within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2024 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 23 May 2024
;; Homepage: N/A
;; Keywords: jira, atlassian
;; Package-Requires: ((emacs "28"))
;; SPDX-License-Identifier: MIT
;; Version: 0.0.1

;;; Commentary:

;; Interact with JIRA

;;; Code:
(require 'bd-jira-issue)
(require 'bd-jira-org)
(require 'bd-jira-board)
(require 'bd-jira-user)
(require 'bd-jira-config)
(require 'subr-x)

;;;;;;;;;;;;;;;;
;;; coloring

(defun bd-jira-view--colorize (text color)
  "Colorize TEXT with COLOR."
  (propertize text 'face `(:foreground ,color)))

(defun bd-jira-view--type-color (type)
  "Return a color for a jira TYPE."
  (cond ((equal type "Epic") "cyan")
	((equal type "Story") "green")
	((equal type "Task") "grey")
	((equal type "Sub-task") "light grey")
	((equal type "Bug") "red")
	(t nil)))

(defun bd-jira-view--status-color (status)
  "Return a color for a jira STATUS."
  (cond ((equal status "Selected for Development") "green")
	((equal status "Done") "red")
	((equal status "In Progress") "orange")
	((equal status "Backlog") "grey")
	((equal status "Ready for QA") "green")
	(t "cyan")))

(defun bd-jira-view--sprint-state-color (state)
  "Generate a color for sprint STATE."
  (cond ((equal state "active") "green")
	((equal state "future") "light blue")
	((equal state "closed") "orange")))

(defun bd-jira-view--sprints-string (sprints-data)
  "Display sprints from SPRINTS-DATA in a human readable way."
  (if sprints-data
      (string-join
       (mapcar
	(lambda (sprint)
	  (cl-destructuring-bind (&key name state &allow-other-keys) sprint
	    (let ((color (bd-jira-view--sprint-state-color state)))
	      (bd-jira-view--colorize name color))))
	sprints-data)
       ", ")
    "None"))

(defun bd-jira-view--title-string (issue-key parent summary)
  "Compute the title string for ISSUE-TYPE ISSUE-KEY PARENT and SUMMARY."
  (let* ((parent? (and parent (not (equal parent ""))))
	 (issue-string (bd-jira-view--format-issue-key issue-key))
	 (parent-string (when parent? (bd-jira-view--format-issue-key parent)))
	 (key-string (if parent? (format "%s/%s" parent-string issue-string) issue-string)))
    (format "#+title: %s: %s" key-string summary)))

;;;;;;;;;;;;;;;;
;;; Single Issue

(defconst bd-jira-view--input-buffer "*benedict-input*")
(defvar bd-jira-view--input-data '())
(defvar bd-jira-view--saved-layout ())

(defun bd-jira-view/insert-code-block (language)
  "Insert a code block for LANGUAGE."
  (interactive "slanguage: ")
  (insert (string-join
	   (list (format "{code:%s}" language)
		 ""
		 ("{code}"))
	   "\n")))

(defun bd-jira-view--resolve-user (user-block)
  "Resolve a user name from USER-BLOCK - unfinished."
  (let (id)
    (save-match-data
      (and (string-match "\\[~accountid:\\([0-9a-z:-]*\\)\\]" user-block)
	   (setq id (match-string 1 user-block))))
    (alist-get 'emailAddress (bd-jira-user/from-id id))))

(defun bd-jira-view/insert-code (code)
  "Format CODE as code."
  (interactive "svalue: ")
  (insert (format "{{%s}}" code)))

(define-minor-mode bd-jira-view/input-mode
    "Minor mode to get input in benedict jira."
  :init-value nil
  :lighter " benedict"
  :keymap `((,(kbd "C-x c") . bd-jira-view/insert-code)
	    (,(kbd "C-x b") . bd-jira-view/insert-code-block)
	    (,(kbd "C-c C-c") . bd-jira-view--send-input-buffer)))

(defun bd-jira-view--send-input-buffer ()
  "Call the add comment function with the appropriate args from buffer."
  (interactive)
  (let* ((buffer (switch-to-buffer bd-jira-view--input-buffer))
	 (to-send (buffer-substring-no-properties (point-min) (point-max)))
	 (key (plist-get bd-jira-view--input-data :key)))
    (cl-case (plist-get bd-jira-view--input-data :operation)
      (:comment (bd-jira-issue/add-comment key to-send))
      (:update-description (bd-jira-issue/update-description
			    key (bd-jira-view--org->jira-md to-send)))
      (:add-label (bd-jira-issue/add-label key to-send)))
    (set-window-configuration bd-jira-view--saved-layout)
    (setq bd-jira-view--saved-layout nil)
    (sleep-for 4)
    (kill-buffer buffer)
    (benedict-jira-view/issue-detail key)))

(defun bd-jira-view/refresh ()
  "Refresh the current view."
  (interactive)
  (benedict-jira-view/issue-detail
   (plist-get bd-jira-view--input-data :key)))

(defun bd-jira-view/quit ()
  "Quit the current view."
  (interactive)
  (setq bd-jira-view--saved-layout nil
	bd-jira-view--input-data nil)
  (kill-buffer nil))

(define-derived-mode bd-jira-view/issue-mode org-mode "JIRA Issue")

(defmacro bd-jira-view--with-issue-buffer (issue-key &rest body)
  "Execute BODY within the for ISSUE-KEY buffer."
  (declare (indent defun))
  `(progn
     (switch-to-buffer (format "*benedict issue: %s*" ,issue-key))
     (read-only-mode -1)
     (delete-region (point-min) (point-max))
     (progn ,@body)
     (read-only-mode 1)
     (bd-jira-view/issue-mode)
     (goto-char (point-min))))

(defun bd-jira-view--quote (string)
  "Ensure that STRING is insertable in rog mode."
  (if string
      (string-join
       (mapcar (lambda (line) (format " %s" line)) (split-string string "\n"))
       "\n")
    ""))

(defun bd-jira-view--jira-md->org (string)
  "Use pandoc to convert STRING to org mode if available."
  ;; maybe cache pandoc exetule check
  (let ((executable (string-trim (shell-command-to-string "which pandoc"))))
    (if (equal executable "")
	(bd-jira-view--quote string)
      (bd-jira-view--quote
       (shell-command-to-string
	(format "echo \"%s\" | %s -f jira -t org"
		(string-replace
		 "{code"
		 "{noformat"
		 (string-replace "`" "'" string))
		executable))))))

(defun bd-jira-view--format-issue-key (issue-key)
  "Format ISSUE-KEY.
Make an org mode link if :domain is available in jira configuration."
  (if-let ((domain (bd-jira-config/get :domain)))
      (format "[[https://%s/browse/%s][%s]]" domain issue-key issue-key)
    issue-key))

(defun bd-jira-view--org->jira-md (string)
  "Use pandoc to convert STRING to jira-md."
  (let ((executable (string-trim (shell-command-to-string "which pandoc"))))
    (if (equal executable "")
	(bd-jira-view--quote string)
      (bd-jira-view--quote
       (shell-command-to-string
	(format "echo \"%s\" | %s -f jira -t org" string executable))))))

(defun bd-jira-view--format-related (related-issues)
  "Format any issues in RELATED-ISSUES to be displayed."
  (when related-issues
    (let ((formatted-relations '()))
      (dolist (relation-binding related-issues)
	(let ((relation-name (format "- *%s*" (car relation-binding)))
	      (relata (string-join
		       (mapcar
			#'bd-jira-view--format-issue-key
			(cdr relation-binding))
		       ",")))
	  (push (format "%s %s" relation-name relata) formatted-relations)))
      (string-join formatted-relations "\n"))))

;;view

(defun bd-jira-view--snoc (item lista)
  "Add ITEM to end of LISTA."
  (reverse (cons item (reverse lista))))

(defun bd-jira-view/display-issue-detail (issue)
  "Draw a view of ISSUE in a new buffer."
  (cl-destructuring-bind (&key
			  sprints key parent assignee reporter
			  type summary comments priority status
			  related description &allow-other-keys)
      issue
    (setq bd-jira-view--input-data issue)
    (bd-jira-view--with-issue-buffer key
	(delete-region (point-min) (point-max))
      (let* ((result
	      (list
	       "#+startup: overview"
	       (bd-jira-view--title-string key parent summary)
	       (format "#+author: %s" reporter)
	       (format "- *type* %s" type)
	       (format "- *status* %s" status)
	       (format "- *priority* %s" priority)
	       (format "- *assigned-to* %s" assignee)
	       (format "- *sprints* %s" (bd-jira-view--sprints-string sprints))
	       (if related (bd-jira-view--format-related related) "")
	       ""
	       "* description"
	       (bd-jira-view--jira-md->org description))))
	(when comments
	  (setq result (bd-jira-view--snoc
			(bd-jira-view--colorize "* comments" "yellow")
			result))
	  (dolist (comment-spec comments)
	    (cl-destructuring-bind (&key comment author created &allow-other-keys)
		comment-spec
	      (setq result
		    (thread-last result
				 (bd-jira-view--snoc (format "** %s: %s" created author))
				 (bd-jira-view--snoc
				  (bd-jira-view--jira-md->org comment)))))))
	(insert (string-join result "\n"))
	(org-mode)))))

(defun benedict-jira-view/issue-detail (issue-key)
  "View ISSUE-KEY with benedicts viewer."
  (interactive)
  (bd-jira-view/display-issue-detail
   (benedict-jira-issue/detail issue-key)))

;; edit
(defun bd-jira-view--get-input (&optional initial-input)
  "Generate a new buffer to get input.
Optionally pass INITIAL-INPUT to populate the buffer."
  (setq bd-jira-view--saved-layout (current-window-configuration))
  (select-window (split-window-below nil (frame-root-window)))
  (switch-to-buffer bd-jira-view--input-buffer)
  (delete-region (point-min) (point-max))
  (when initial-input
    (insert initial-input)
    (goto-char (point-min)))
  (when (member (plist-get bd-jira-view--input-data :operation) (list :update-description))
    (org-mode))
  (bd-jira-view/input-mode 1))

(defun bd-jira-view/add-comment ()
  "Add a comment to the current issue."
  (interactive)
  (plist-put bd-jira-view--input-data :operation :comment)
  (bd-jira-view--get-input))

(defun bd-jira-view/update-description ()
  "Update the description of the current issue."
  (interactive)
  (plist-put bd-jira-view--input-data :operation :update-description)
  (bd-jira-view--get-input
   (bd-jira-view--jira-md->org
    (plist-get bd-jira-view--input-data :description))))

(defun bd-jira-view/add-label ()
  "Add a label to the current issue."
  (interactive)
  (plist-put bd-jira-view--input-data :operation :add-label)
  (bd-jira-view--get-input))

(defun bd-jira-view/add-link (relation relatum)
  "Add a link from the current issue to RELATUM via RELATION."
  (interactive
   (list
    (completing-read "relation: " *bd-jira-issue/link-types* nil t)
    (read-string "relatum: ")))
  (let ((issue-key (plist-get bd-jira-view--input-data :key)))
    (bd-jira-issue/add-link issue-key relation relatum)))

(defun bd-jira-view/update-status (status)
  "Update the status of the current issue with STATUS."
  (interactive
   (list
    (completing-read "new status: "
		     (mapcar #'car *bd-jira-issue/transitions*)
		     nil
		     t)))
  (let ((key (plist-get bd-jira-view--input-data :key)))
    (bd-jira-issue/update-status key status)))

(defun bd-jira-view--sprint->id (sprint-spec)
  "Convert SPRINT-SPEC into an alist entry of a formatted name to an id."
  (cl-destructuring-bind (&key name state id &allow-other-keys) sprint-spec
    (let ((state-string (bd-jira-view--colorize
			 state (bd-jira-view--sprint-state-color state))))
      (cons (format "%s: %s" state-string name) id))))

(defun bd-jira-view/add-issue-to-sprint ()
  "Update the sprint on the current issue."
  (interactive)
  (let* ((key (plist-get bd-jira-view--input-data :key))
	 (sprints (mapcar #'bd-jira-view--sprint->id
			  (seq-filter
			   (lambda (sprint)
			     (not (equal (plist-get sprint :state) "closed")))
			   (bd-jira-board/sprints bd-jira-org/board-ids))))
	 (selected-sprint (completing-read "sprint: " sprints nil t))
	 (sprint-id (alist-get selected-sprint sprints nil nil #'equal)))
    ;; this should be at a lower level of abstraction
    (benedict/issue-update key :sprint sprint-id)))

(defun bd-jira-view--user-search ()
  "Search jira for user."
  (let* ((person (bd-jira-user/search (read-string "search by: ")))
	 (account-id (alist-get 'accountId person))
	 (name (alist-get 'displayName person))
	 (email (alist-get 'emailAddress person)))
    (when (y-or-n-p (format "Found %s (%s).  Is this correct? " name email))
      account-id)))


(defun bd-jira-view/assign ()
  "Assign the current issue to a user."
  (interactive)
  (let* ((key (plist-get bd-jira-view--input-data :key))
	 (assignee (if (y-or-n-p "Assign to current user?")
		       (progn
			 (bd-jira-user/set-user)
			 (bd-jira-config/get :account-id))
		     (bd-jira-view--user-search))))
    (when assignee
      (bd-jira-issue/assign key assignee))))

(defun bd-jira-view/capture (org-file outline-path)
  "Save the current issue to an ORG-FILE at OUTLINE-PATH.
Sets the default priority to A."
  (let* ((key (plist-get bd-jira-view--input-data :key))
	 (template (string-join
		    (list
		     "* todo [#A] %? %(org-set-tags \""
		     (string-replace "-" "_" key)
		     "\")\nSCHEDULED: %^t %^{EFFORT}p")))
	 (old-templates org-capture-templates))
    (setq org-capture-templates (list
				 `("c" "JIRA Capture" entry
				       (file+olp ,org-file ,@outline-path)
				       ,template)))
    (unwind-protect
	 (org-capture nil "c")
      (setq org-capture-templates old-templates))))

(defun bd-jira-view/follow-link ()
  "Follow a jira link at point."
  (interactive)
  (let ((word (current-word)))
    (when (string-match "[A-Z]+-[0-9]+" word)
      (benedict-jira-view/issue-detail
       (string-trim (substring-no-properties (match-string 0 word)))))))

      ;;;
;;;;;;;;;

(define-derived-mode bd-jira-view/issues-mode fundamental-mode
  "View JIRA issues"
  "Major mode for viewing jira issues."
  (define-key bd-jira-view/issues-mode-map
      (kbd "C-c C-f") #'bd-jira-view/add-filter)
  (define-key bd-jira-view/issues-mode-map
      (kbd "C-c C-q") #'bd-jira-view/quit)
  (define-key bd-jira-view/issues-mode-map
      (kbd "C-c C-o") #'bd-jira-view/detail-at-point)
  (define-key bd-jira-view/issues-mode-map
      (kbd "C-c C-s") #'bd-jira-view/sort-by)
  (define-key bd-jira-view/issues-mode-map
      (kbd "C-c C-x") #'bd-jira-view/remove-filters))

(defvar bd-jira-view--issues-data nil
  "A place to store data about issues.")

(defmacro bd-jira-view--with-issues-buffer (data &rest body)
  "Execute BODY within the DATA stored in bd-jira-view--issues-data."
  (declare (indent defun))
  `(progn
     (switch-to-buffer "*benedict issues view*")
     ;; I don't understand why `setq-local` doesn't work here
     (setq bd-jira-view--issues-data ,data)
     (read-only-mode -1)
     (delete-region (point-min) (point-max))
     (progn ,@body)
     (read-only-mode 1)
     (bd-jira-view/issues-mode)
     (goto-char (point-min))))

(defun bd-jira-view--column->max (columns rows)
  "Get the max char length of COLUMNS for in ROWS."
  (let ((result '()))
    (dolist (column columns)
      (let ((old-val (or (plist-get result column) 0))
	    (current-val (- (length (format "%s" column)) 1)))
	(when (< old-val current-val)
	  (setq result (plist-put result column current-val)))))
    (dolist (row rows)
      (dolist (column columns)
	(let ((old-val (plist-get result column))
	      (current-val (length (plist-get row column))))
	  (when (< old-val current-val)
	    (setq result (plist-put result column current-val))))))
    result))

(defun bd-jira-view--padding (column->max-size column value)
  "Generate padding for COLUMN with VALUE based on COLUMN->MAX-SIZE."
  (let ((current-size (length value))
	(max-size (plist-get column->max-size column)))
    (make-string (+ (- max-size current-size) 1) ? )))

(defun bd-jira-view--generate-table-string
    (columns rows column->color-fn)
  "Genereate the table for COLUMNS and ROWS, with COLUMN->COLOR-FN."
  (let ((column->size (bd-jira-view--column->max columns rows))
	(header "")
	(body  '()))
    ;; headers
    (dolist (column columns)
      (let* ((cell (substring (format "%s" column) 1))
	     (padding (bd-jira-view--padding column->size column cell)))
	(setq header (concat header cell padding))))
    ;; rows
    (dolist (row rows)
      (let ((row-string ""))
	(dolist (column columns)
	  (let* ((cell-content (or (plist-get row column) ""))
		 (padding (bd-jira-view--padding
			   column->size column cell-content))
		 (color-fn (plist-get column->color-fn column))
		 (color (when color-fn (funcall color-fn cell-content)))
		 (cell-string (concat cell-content padding))
		 (cell (if color
			   (bd-jira-view--colorize cell-string color)
			 cell-string)))
	    (setq row-string (concat row-string cell))))
	(push row-string body)))
    (let ((divider (make-string (length header) ?=)))
      (string-join (cons header (cons divider body)) "\n"))))

(defun bd-jira-view--view-issues-table
    (original-data columns rows &optional column->color-fn)
  "Create a table for viewing ROWS that have COLUMNS and ORIGINAL-DATA."
  (bd-jira-view--with-issues-buffer original-data
      (insert (bd-jira-view--generate-table-string columns rows column->color-fn))))

(defun bd-jira-view--truncate (string width)
  "Truncate STRING to WIDTH."
  (if (> (length string) width)
      (format "%s..." (substring string 0 (- width 3)))
    string))

(defvar bd-jira-view--issue-list-columns
  (list :type :key :status :summary :assignee :reporter))

(defun bd-jira-view--process-issues (issues)
  "Ensure that issue summaries in ISSUES is truncated."
  (mapcar
   (lambda (issue)
     (cl-destructuring-bind (&key summary &allow-other-keys)
	 issue
       (plist-put issue :summary (bd-jira-view--truncate summary 100))))
   issues))

(defvar bd-jira-view--color-issues-spec
  (list
   :type #'bd-jira-view--type-color
   :status #'bd-jira-view--status-color
   :key (lambda (x) "yellow")))

(defun bd-jira-view/issues (issues)
  "View ISSUES as a table."
  (let ((columns bd-jira-view--issue-list-columns)
	(rows (bd-jira-view--process-issues issues))
	(metadata (list :original issues :current issues)))
    (bd-jira-view--view-issues-table metadata columns rows bd-jira-view--color-issues-spec)))

(defun bd-jira-view--keyword->name (keyword)
  "Get the name of KEYWORD."
  (substring (format "%s" keyword) 1))

(defun bd-jira-view--column-name->column ()
  "Create a map from column names to the keywords representing them."
  (mapcar
   (lambda (symbol)
     (cons (bd-jira-view--keyword->name symbol)
	   symbol))
   bd-jira-view--issue-list-columns))

(defun bd-jira-view/add-filter ()
  "Add a filter to the current view."
  (interactive)
  (cl-destructuring-bind (&key current original)
      bd-jira-view--issues-data
    (let* ((filter-col (alist-get
			(completing-read
			 "select filter column: "
			 (mapcar
			  #'bd-jira-view--keyword->name
			  bd-jira-view--issue-list-columns)
			 nil t)
			(bd-jira-view--column-name->column)
			nil nil #'equal))
	   (values (mapcar
		    (lambda (issue) (plist-get issue filter-col))
		    current))
	   (filter-group (mapcar
			  #'downcase
			  (completing-read-multiple "filter on (comma separated): " values)))
	   (new-issues (seq-filter
			(lambda (issue)
			  (thread-first
			    issue
			    (plist-get filter-col)
			    downcase
			    (member filter-group)))
			current)))
      ;; put a place to save the original
      (bd-jira-view--view-issues-table
       (list :current new-issues :original original)
       bd-jira-view--issue-list-columns
       new-issues
       bd-jira-view--color-issues-spec))))

(defun bd-jira-view/remove-filters ()
  "Remove any filters from the current view."
  (interactive)
  (cl-destructuring-bind (&key original &allow-other-keys)
      bd-jira-view--issues-data
    (bd-jira-view--view-issues-table
     (list :current original :original original)
     bd-jira-view--issue-list-columns
     original
     bd-jira-view--color-issues-spec)))

(defun bd-jira-view/sort-by ()
  "Sort the current view."
  (interactive)
  (cl-destructuring-bind (&key current original)
      bd-jira-view--issues-data
    (let* ((sort-col (alist-get
			(completing-read
			 "select sort column: "
			 (mapcar
			  #'bd-jira-view--keyword->name
			  bd-jira-view--issue-list-columns)
			 nil t)
			(bd-jira-view--column-name->column)
			nil nil #'equal))
	   (new-issues (seq-sort
			(lambda (issue-one issue-two)
			  (string< (plist-get issue-one sort-col)
				   (plist-get issue-two sort-col)))
			current)))
      (bd-jira-view--view-issues-table
       (list :current new-issues :original original)
       bd-jira-view--issue-list-columns
       new-issues
       bd-jira-view--color-issues-spec))))

(defun bd-jira-view/detail-at-point ()
  "Create a new buffer with a detailed view of the issue at point."
  (interactive)
  (let ((line (thing-at-point 'line)))
    (when (string-match "\\s-+[A-Z]+-[0-9]+\\s-+" line)
      (benedict-jira-view/issue-detail
       (string-trim (substring-no-properties (match-string 0 line)))))))

(provide 'bd-jira-view)
;;; bd-jira-view.el ends here
