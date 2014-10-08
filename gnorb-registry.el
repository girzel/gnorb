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
Message-ID strings, with angle brackets, or a single string of
Message-IDs), produce a list of Org ids for headings that are
relevant to that message."
  (let (ret-val sub-val)
    (when (stringp ids)
      (setq ids (gnus-extract-references ids)))
    (when gnorb-tracking-enabled
      (setq ids (delete-dups ids))
      (progn
	(dolist (id ids)
	  (when
	      (setq sub-val
		    (gnus-registry-get-id-key id 'gnorb-ids))
	    (setq ret-val (append sub-val ret-val))))))
    (delete-dups ret-val)))

(defun gnorb-registry-org-id-search (id)
  "Find all messages that have the org ID in their 'gnorb-ids
key."
  (registry-search gnus-registry-db :member `((gnorb-ids ,id))))

(defun gnorb-registry-transition-from-props (arg)
  "Helper function for transitioning the old tracking system to the new.

The old system relied on storing sent message ids on relevant Org
headings, in the `gnorb-org-msg-id-key' property. The new system
uses the gnus registry to track relations between messages and
Org headings. This function will go through your agenda files,
find headings that have the `gnorb-org-msg-id-key' property set,
and create new registry entries that reflect that connection.

Call with a prefix arg to additionally delete the
`gnorb-org-msg-id-key' altogether from your Org headings. As this
function will not create duplicate registry entries, it's safe to
run it once with no prefix arg, to keep the properties in place,
and then once you're sure everything's working okay, run it again
with a prefix arg, to clean the Gnorb-specific properties from
your Org files."
  (interactive "P")
  (let ((count 0))
    (message "Collecting all relevant Org headings, this could take a while...")
    (org-map-entries
     (lambda ()
       (let ((id (org-id-get))
	     (props (org-entry-get-multivalued-property
	       (point) gnorb-org-msg-id-key))
	     links group id)
	(when props
	  ;; If the property is set, we should probably assume that any
	  ;; Gnus links in the subtree are relevant, and should also be
	  ;; collected and associated.
	  (setq links (gnorb-scan-links
		       (org-element-property :end (org-element-at-point))
		       'gnus))
	  (dolist (l (plist-get links :gnus))
	    (gnorb-registry-make-entry
	     (second (split-string l "#")) nil nil
	     id (first (split-string l "#"))))
	  (dolist (p props)
	    (setq id )
	    (gnorb-registry-make-entry p nil nil id nil)
	    ;; This function will try to find the group for the message
	    ;; and set that value on the registry entry if it can find
	    ;; it.
	    (unless (gnus-registry-get-id-key p 'group)
	      (gnorb-msg-id-to-group p))
	    (incf count)))))
     gnorb-org-find-candidates-match
     'agenda 'archive 'comment)
    (message "Collecting all relevant Org headings, this could take a while... done")
    ;; Delete the properties if the user has asked us to do so.
    (if (equal arg '(4))
	(progn
	  (dolist (f (org-agenda-files))
	    (with-current-buffer (get-file-buffer f)
	      (org-delete-property-globally gnorb-org-msg-id-key)))
	  (message "%d entries created; all Gnorb-specific properties deleted."
		   count))
      (message "%d entries created." count))))

(provide 'gnorb-registry)
