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
    (gnus-registry-set-id-key msg-id 'sender (list sender))
    (gnus-registry-set-id-key msg-id 'subject (list subject))
    (gnus-registry-set-id-key msg-id 'gnorb-ids (if (stringp org-id)
						    (list org-id)
						  org-id))
    (gnus-registry-set-id-key msg-id 'group (list group))))

(defun gnorb-registry-capture ()
  "When capturing from a Gnus message, add our new Org heading id
to the message's registry entry, under the 'gnorb-ids key."
  (when (and (with-current-buffer
		 (org-capture-get :original-buffer)
	       (memq major-mode '(gnus-summary-mode gnus-article-mode)))
	     (not org-note-abort))
    (let* ((msg-id
	    (concat "<" (plist-get org-store-link-plist :message-id) ">"))
	   (entry (gnus-registry-get-or-make-entry msg-id))
	   (org-ids
	    (gnus-registry-get-id-key msg-id 'gnorb-ids))
	   (new-org-id (org-id-get-create)))
      (setq org-ids (cons new-org-id org-ids))
      (setq org-ids (delete-dups org-ids))
      (gnus-registry-set-id-key msg-id 'gnorb-ids org-ids))))

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

(provide 'gnorb-registry)
