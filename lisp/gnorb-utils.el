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

(make-obsolete-variable
 'gnorb-trigger-todo-default
 "This variable has been superseded by
`gnorb-org-trigger-actions'"
 "September 8, 2014" 'set)

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
  ;; appropriately, then sets it to nil.
  )

(defvar gnorb-window-conf nil
  "Save window configurations here, for restoration after mails
are sent, or Org headings triggered.")

(defvar gnorb-return-marker (make-marker)
  "Return point here after various actions, to be used together
with `gnorb-window-conf'.")

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

(defun gnorb-restore-layout ()
  "Restore window layout and value of point after a Gnorb command.

Some Gnorb commands change the window layout (ie `gnorb-org-view'
or incoming email triggering). This command restores the layout
to what it was. Bind it to a global key, or to local keys in Org
and Gnus and BBDB maps."
  (interactive)
  (when (window-configuration-p gnorb-window-conf)
    (set-window-configuration gnorb-window-conf)
    (when (buffer-live-p (marker-buffer gnorb-return-marker))
      (goto-char gnorb-return-marker))))

(defun gnorb-trigger-todo-action (arg &optional id)
  "Do the actual restore action. Two main things here. First: if
we were in the agenda when this was called, then keep us in the
agenda. Then let the user choose an action from the value of
`gnorb-org-trigger-actions'."
  (let ((agenda-p (eq major-mode 'org-agenda-mode))
	(action (cdr (assoc
		      (org-completing-read
		       "Action to take: "
		       gnorb-org-trigger-actions nil t)
		      gnorb-org-trigger-actions)))
	(root-marker (make-marker)))
    ;; Place the marker for the relevant TODO heading.
    (cond (agenda-p
	   (setq root-marker
		 (copy-marker
		  (org-get-at-bol 'org-hd-marker))))
	  ((derived-mode-p 'org-mode)
	   (move-marker root-marker (point-at-bol)))
	  (id
	   (save-excursion
	     (org-id-goto id)
	     (move-marker root-marker (point-at-bol)))))
    ;; Query about attaching email attachments.
    (org-with-point-at root-marker
      (map-y-or-n-p
       (lambda (a)
	 (format "Attach %s to heading? "
		 (file-name-nondirectory a)))
       (lambda (a) (org-attach-attach a nil 'mv))
       gnorb-gnus-capture-attachments
       '("file" "files" "attach")))
    (setq gnorb-gnus-capture-attachments nil)
    (cl-labels
	((make-entry
	  (id)
	  (gnorb-registry-make-entry
	   (plist-get gnorb-gnus-message-info :msg-id)
	   (plist-get gnorb-gnus-message-info :from)
	   (plist-get gnorb-gnus-message-info :subject)
	   id
	   (plist-get gnorb-gnus-message-info :group))))
      ;; Handle our action.
      (cond ((eq action 'note)
	     (org-with-point-at root-marker
	       (make-entry (org-id-get-create))
	       (call-interactively 'org-add-note)))
	    ((eq action 'todo)
	     (if agenda-p
		 (progn
		   (org-with-point-at root-marker
		    (make-entry (org-id-get-create)))
		   (call-interactively 'org-agenda-todo))
	       (org-with-point-at root-marker
		 (make-entry (org-id-get-create))
		 (call-interactively 'org-todo))))
	    ((eq action 'no-associate)
	     nil)
	    ((eq action 'associate)
	     (org-with-point-at root-marker
	       (make-entry (org-id-get-create))))
	    ((fboundp action)
	     (org-with-point-at root-marker
	       (make-entry (org-id-get-create))
	       (funcall action gnorb-gnus-message-info)))))))

(defun gnorb-pretty-outline (id &optional kw)
  "Return pretty outline path of the Org heading indicated by ID.

If the KW argument is true, add the TODO keyword into the path."
  (org-with-point-at (org-id-find id t)
    (let ((el (org-element-at-point)))
      (concat
       (if kw
	   (format "(%s): "
		   (org-element-property :todo-keyword el))
	 "")
       (org-format-outline-path
	(append
	 (list
	  (file-name-nondirectory
	   (buffer-file-name
	    (org-base-buffer (current-buffer)))))
	 (org-get-outline-path)
	 (list
	  (replace-regexp-in-string
	   org-bracket-link-regexp
	   "\\3" (org-element-property :raw-value el)))))))))

(defun gnorb-scan-links (bound &rest types)
  "Scan from point to BOUND looking for links of type in TYPES.

TYPES is a list of symbols, possible values include 'bbdb, 'mail,
and 'gnus."
  ;; this function could be refactored somewhat -- lots of code
  ;; repetition. It also should be a little faster for when we're
  ;; scanning for gnus links only, that's a little slow. We should
  ;; probably use a different regexp based on the value of TYPES.
  ;;
  ;; This function should also *not* be responsible for unescaping
  ;; links -- we don't know what they're going to be used for, and
  ;; unescaped is safer.
  (unless (= (point) bound)
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
      `(:gnus ,(reverse gnus) :mail ,(reverse mail) :bbdb ,(reverse bbdb)))))

(defun gnorb-msg-id-to-link (msg-id)
  "Given a message id, try to create a full org link to the
message."
  (let ((server-group (gnorb-msg-id-to-group msg-id)))
    (when server-group
      (org-link-escape (concat server-group "#" msg-id)))))

(defun gnorb-msg-id-to-group (msg-id)
  "Given a message id, try to find the group it's in.

So far we're checking the registry, then the groups in
`gnorb-gnus-sent-groups'. Use search engines? Other clever
methods?"
  (let (candidates server-group)
    (catch 'found
      (when gnorb-tracking-enabled
	;; Make a big list of all the groups where this message might
	;; conceivably be.
	(setq candidates
	      (append (gnus-registry-get-id-key msg-id 'group)
		      gnorb-gnus-sent-groups))
	(while (setq server-group (pop candidates))
	  (when (and (stringp server-group)
		     (not
		      (string-match-p
		       "\\(nnir\\|nnvirtual\\|UNKNOWN\\)"
		       server-group))
		     (ignore-errors
		       (gnus-request-head msg-id server-group)))
		(throw 'found server-group))))
      (when (featurep 'notmuch)
	nil))))

(defun gnorb-collect-ids (&optional id)
  "Collect all Org IDs for a subtree.

Starting with the heading under point (or the heading indicated
by the ID argument), collect its ID property, and the IDs of all
child headings."
  (save-excursion
    (save-restriction
      (when id
	(org-id-goto id))
      (org-narrow-to-subtree)
      (org-element-map (org-element-parse-buffer)
	  'headline
	(lambda (hl)
	  (org-element-property :ID hl))))))

;; Loading the registry

(defvar gnorb-tracking-enabled nil
  "Internal flag indicating whether Gnorb is successfully plugged
  into the registry or not.")

(defun gnorb-tracking-initialize ()
  "Start using the Gnus registry to track correspondences between
Gnus messages and Org headings. This requires that the Gnus
registry be in use, and should be called after the call to
`gnus-registry-initialize'."
  (require 'gnorb-registry)
  (add-hook
   'gnus-started-hook
   (lambda ()
     (unless (gnus-registry-install-p)
       (user-error "Gnorb tracking requires that the Gnus registry be installed."))
     (add-to-list 'gnus-registry-extra-entries-precious 'gnorb-ids)
     (add-to-list 'gnus-registry-track-extra 'gnorb-ids)
     (add-hook 'org-capture-mode-hook 'gnorb-registry-capture)
     (add-hook 'org-capture-prepare-finalize-hook 'gnorb-registry-capture-abort-cleanup)
     (setq gnorb-tracking-enabled t))))

(provide 'gnorb-utils)
;;; gnorb-utils.el ends here
