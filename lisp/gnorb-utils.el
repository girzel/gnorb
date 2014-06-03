;;; gnorb-utils.el --- Common utilities for all gnorb stuff.

;; Copyright (C) 2014  Eric Abrahamsen

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl)
(require 'mailcap)
(require 'gnus)
;(require 'message)
(require 'bbdb)
(require 'org)
(require 'org-bbdb)
(require 'org-gnus)

(mailcap-parse-mimetypes)

(defgroup gnorb nil
  "Glue code between Gnus, Org, and BBDB."
  :tag "Gnorb")

(defcustom gnorb-trigger-todo-default 'prompt
  "What default action should be taken when triggering TODO
  state-change from a message? Valid values are the symbols note
  and todo, or prompt to pick one of the two."
  :group 'gnorb
  :type '(choice (const note)
		 (const todo)
		 (const prompt)))

(defun gnorb-prompt-for-bbdb-record ()
  "Prompt the user for a BBDB record."
  (let ((recs (bbdb-records))
	name)
    (while (> (length recs) 1)
      (setq name
	    (completing-read
	     (format "Filter records by regexp (%d remaining): "
		     (length recs))
	     (mapcar 'bbdb-record-name recs)))
      (setq recs (bbdb-search recs name name name nil nil)))
    (if recs
	(car recs)
      (error "No matching records"))))

(defvar gnorb-tmp-dir (make-temp-file "emacs-gnorb" t)
  "Temporary directory where attachments etc are saved.")

(defvar gnorb-message-org-ids nil
  "List of Org heading IDs from the outgoing Gnus message, used
  to mark mail TODOs as done once the message is sent."
  ;; The send hook either populates this, or sets it to nil, depending
  ;; on whether the message in question has an Org id header. Then
  ;; `gnorb-org-restore-after-send' checks for it and acts
  ;; appropriately.
)

(defcustom gnorb-mail-header "X-Org-ID"
  "Name of the mail header used to store the ID of a related Org
  heading. Only used locally: always stripped when the mail is
  sent."
  :group 'gnorb
  :type 'string)

;;; this is just ghastly, but the value of this var is single regexp
;;; group containing various header names, and we want our value
;;; inside that group.
(eval-after-load 'message
  `(let ((ign-headers-list
	  (split-string message-ignored-mail-headers
			"|"))
	 (our-val (concat gnorb-mail-header "\\")))
     (unless (member our-val ign-headers-list)
       (setq ign-headers-list
	     `(,@(butlast ign-headers-list 1) ,our-val
	       ,@(last ign-headers-list 1)))
       (setq message-ignored-mail-headers
	     (mapconcat
	      'identity ign-headers-list "|")))))

(defun gnorb-trigger-todo-action (arg &optional id)
  "Do the actual restore action. Two main things here. First: if
we were in the agenda when this was called, then keep us in the
agenda. Second: try to figure out the correct thing to do once we
reach the todo. That depends on `gnorb-trigger-todo-default', and
the prefix arg."
  (let* ((agenda-p (eq major-mode 'org-agenda-mode))
	 (todo-func (if agenda-p
			'org-agenda-todo
		      'org-todo))
	 (note-func (if agenda-p
			'org-agenda-add-note
		      'org-add-note))
	 root-marker ret-dest-todo action)
    (when (and (not agenda-p) id)
      (org-id-goto id))
    (setq root-marker (if agenda-p
			  (org-get-at-bol 'org-hd-marker)
			(point-at-bol))
	  ret-dest-todo (org-entry-get
			 root-marker "TODO"))
    (let ((ids (org-entry-get-multivalued-property
		root-marker gnorb-org-msg-id-key))
	  (sent-id (plist-get gnorb-gnus-sending-message-info :msg-id)))
      (when sent-id
	(org-entry-add-to-multivalued-property
	 root-marker gnorb-org-msg-id-key sent-id))
      (setq action (cond ((not
			   (or (and ret-dest-todo
				    (null gnorb-org-mail-todos))
			       (member ret-dest-todo gnorb-org-mail-todos)))
			  'note)
			 ((eq gnorb-trigger-todo-default 'prompt)
			  (intern (completing-read
				   "Take note, or trigger TODO state change? "
				   '("note" "todo") nil t)))
			 ((null arg)
			  gnorb-trigger-todo-default)
			 (t
			  (if (eq gnorb-trigger-todo-default 'todo)
			      'note
			    'todo))))
      (if (eq action 'note)
	  (call-interactively note-func)
	(call-interactively todo-func)))))

(defsubst gnorb-bbdb-link-to-mail (link)
  (with-current-buffer bbdb-buffer-name
    (let ((recs bbdb-records))
      (org-open-link-from-string (concat "[[bbdb:" link "]]"))
      (let ((mail (bbdb-mail-address (bbdb-current-record))))
	(bbdb-display-records recs)
	mail))))

(defun gnorb-scan-links (bound &rest types)
  ;; this function could be refactored somewhat -- lots of code
  ;; repetition. It also should be a little faster for when we're
  ;; scanning for gnus links only, that's a little slow. We should
  ;; probably use a different regexp based on the value of TYPES.
  ;;
  ;; This function should also *not* be responsible for unescaping
  ;; links -- we don't know what they're going to be used for, and
  ;; unescaped is safer.
  (unless (eq (point) bound)
    (let (addr gnus mail bbdb)
      (while (re-search-forward org-any-link-re bound t)
	(setq addr (or (match-string-no-properties 2)
		       (match-string-no-properties 0)))
	(cond
	 ((and (memq 'gnus types)
	       (string-match "^<?gnus:" addr))
	  (push (substring addr (match-end 0)) gnus))
	 ((and (memq 'mail types)
	       (string-match "^<?mailto:" addr))
	  (push (substring addr (match-end 0)) mail))
	 ((and (memq 'bbdb types)
	       (string-match "^<?bbdb:" addr))
	  (push (substring addr (match-end 0)) bbdb))))
      `(:gnus ,gnus :mail ,mail :bbdb ,bbdb))))

(provide 'gnorb-utils)
;;; gnorb-utils.el ends here
