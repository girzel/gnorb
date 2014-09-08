;;; gnorb-registry.el --- Registry implementation for Gnorb

;; This file is in the public domain.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net.>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Early on, Gnorb's message/todo tracking was done by relying on the
;; user to insert links to received messages into an Org heading, and
;; by automatically storing the Message-Ids of sent messages in a
;; property (`gnorb-org-msg-id-key', defaulting to GNORB_MSG_ID) on
;; the same heading. The heading could find all relevant messages by
;; combining the links (incoming) and the IDs of the Gnorb-specific
;; property (outgoing).
;;
;; In the end, this proved to be fragile and messy. Enter the
;; registry. The Gnus registry is a specialization of a general
;; "registry" library -- it's possible to roll your own. If you want
;; to track connections between messages and Org headings, it's an
;; obvious choice: Each relevant message is stored in the registry,
;; keyed on its Message-ID, and the org-ids of all relevant headings
;; are stored in a custom property, in our case gnorb-ids. This allows
;; us to keep all Gnorb-specific data in one place, without polluting
;; Org files or Gnus messages, persistent on disk, and with the added
;; bonus of providing a place to keep arbitrary additional metadata.
;;
;; The drawback is that the connections are no longer readily visible
;; to the user (they need to query the registry to see them), and it
;; becomes perhaps a bit more difficult (but only a bit) to keep
;; registry data in sync with the current state of the user's Gnus and
;; Org files. But a clear win, in the end.

;;; Code:

(require 'gnus-registry)

(defgroup gnorb-registry nil
  "Gnorb's use of the Gnus registry."
  :tag "Gnorb Registry"
  :group 'gnorb)

(defvar gnorb-msg-id-to-heading-table nil
  "Hash table where keys are message-ids, and values are lists of
  org headings which have that message-id in their GNORB_MSG_ID
  property. Values are actually two-element lists: the heading's
  id, and its outline path.")

(defun gnorb-registry-make-entry (msg-id sender subject org-id group)
  "Create a Gnus registry entry for a message, either received or
sent. Save the relevant Org ids in the 'gnorb-ids key."
  ;; This set-id-key stuff is actually horribly
  ;; inefficient.
  (when gnorb-tracking-enabled
    (gnus-registry-get-or-make-entry msg-id)
    (when sender
      (gnus-registry-set-id-key msg-id 'sender (list sender)))
    (when subject
      (gnus-registry-set-id-key msg-id 'subject (list subject)))
    (when org-id
      (let ((ids (gnus-registry-get-id-key msg-id 'gnorb-ids)))
	(unless (member org-id ids)
	 (gnus-registry-set-id-key msg-id 'gnorb-ids (if (stringp org-id)
							 (cons org-id ids)
						       (append org-id ids))))))
    (when group
      (gnus-registry-set-id-key msg-id 'group (list group)))
    (gnus-registry-get-or-make-entry msg-id)))

(defun gnorb-registry-capture ()
  "When capturing from a Gnus message, add our new Org heading id
to the message's registry entry, under the 'gnorb-ids key."
  (when (and (with-current-buffer
		 (org-capture-get :original-buffer)
	       (memq major-mode '(gnus-summary-mode gnus-article-mode)))
	     (not org-note-abort))
    (let* ((msg-id
	    (format "<%s>" (plist-get org-store-link-plist :message-id)))
	   (entry (gnus-registry-get-or-make-entry msg-id))
	   (org-ids
	    (gnus-registry-get-id-key msg-id 'gnorb-ids))
	   (new-org-id (org-id-get-create)))
      (plist-put org-capture-plist :gnorb-id new-org-id)
      (setq org-ids (cons new-org-id org-ids))
      (setq org-ids (delete-dups org-ids))
      (gnus-registry-set-id-key msg-id 'gnorb-ids org-ids))))


(defun gnorb-registry-capture-abort-cleanup ()
  (when (and (org-capture-get :gnorb-id)
	     org-note-abort)
    (condition-case error
	(let* ((msg-id (format "<%s>" (plist-get org-store-link-plist :message-id)))
	       (existing-org-ids (gnus-registry-get-id-key msg-id 'gnorb-ids))
	       (org-id (org-capture-get :gnorb-id)))
	  (when (member org-id existing-org-ids)
	    (gnus-registry-set-id-key msg-id 'gnorb-ids
				      (remove org-id existing-org-ids)))
	  (setq abort-note 'clean))
      (error
       (setq abort-note 'dirty)))))

(defun gnorb-find-visit-candidates (ids)
  "For all message-ids in IDS (which should be a list of
Message-ID strings, with angle brackets), produce a list of Org
ids (and ol-paths) for headings that contain one of those id
values in their `gnorb-org-org-msg-id-key' property."
  (let (ret-val sub-val)
    (unless gnorb-msg-id-to-heading-table
      (gnorb-org-populate-id-hash))
    (dolist (id ids)
      (when (setq sub-val (gethash id gnorb-msg-id-to-heading-table))
	(setq ret-val (append sub-val ret-val))))
    ret-val))

(defun gnorb-org-add-id-hash-entry (msg-id &optional marker)
  (org-with-point-at (or marker (point))
    (let ((old-val (gethash msg-id gnorb-msg-id-to-heading-table))
	  (new-val (list
		    (org-id-get-create)
		    (append
		     (list
		      (file-name-nondirectory
		       (buffer-file-name
			(org-base-buffer (current-buffer)))))
		     (org-get-outline-path)
		     (list
		      (org-no-properties
		       (replace-regexp-in-string
			org-bracket-link-regexp
			"\\3"
			(nth 4 (org-heading-components)))))))))
      (unless (member (car new-val) old-val)
	(puthash msg-id
		 (if old-val
		     (append (list new-val) old-val)
		   (list new-val))
		 gnorb-msg-id-to-heading-table)))))

(defun gnorb-org-populate-id-hash ()
  "Scan all agenda files for headings with the
  `gnorb-org-msg-id-key' property, and construct a hash table of
  message-ids as keys, and org headings as values -- actually
  two-element lists representing the heading's id and outline
  path."
  ;; where are all the places where we might conceivably want to
  ;; refresh this?
  (interactive)
  (setq gnorb-msg-id-to-heading-table
	(make-hash-table
	 :test 'equal :size 100))
  (let (props)
(defun gnorb-registry-org-id-search (id)
  (registry-search gnus-registry-db :member `((gnorb-ids ,id))))

    (org-map-entries
     (lambda ()
       (setq props
	     (org-entry-get-multivalued-property
	      (point) gnorb-org-msg-id-key))
       (dolist (p props)
	 (gnorb-org-add-id-hash-entry p)))
     gnorb-org-find-candidates-match
     'agenda 'archive 'comment)))

(provide 'gnorb-registry)
