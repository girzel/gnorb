;;; gnorb-org.el --- The Org-centric functions of gnorb

;; Copyright (C) 2014  Eric Abrahamsen

;; Author: Eric Abrahamsen  <eric@ericabrahamsen.net>
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

(require 'gnorb-utils)

(defgroup gnorb-org nil
  "The Org bits of Gnorb."
  :tag "Gnorb Org"
  :group 'gnorb)

(defcustom gnorb-org-mail-todos '("EMAIL" "REPLY")
  "TODO keywords that are considered mail related -- functions in
this library may, after completion, call org-(agenda-)todo on
relevant headings with these keywords. Set to nil to call
org-todo regardless of TODO type."
  :group 'gnorb-org)

(defcustom gnorb-org-after-message-setup-hook nil
  "Hook run in a message buffer after setting up the message from
  `gnorb-org-handle-mail' or `gnorb-org-email-subtree'."
  :group 'gnorb-org
  :type 'hook)

(defcustom gnorb-org-msg-id-key "GNORB_MSG_ID"
  "The name of the org property used to store the Message-IDs
  from relevant messages.")

(defcustom gnorb-org-mail-scan-scope 1
  "When calling `gnorb-org-handle-mail' on a heading, this option
specifies how much of the heading text will be scanned for
relevant message and mail links. Set to 0 to only look within the
heading text itself. Set to an integer to scan that many
paragraphs of the text body. Set to the symbol 'text to scan all
the text immediately under the heading (excluding sub-headings),
and to the symbol 'subtree to scan all the text in the whole
subtree.

Note that if `gnorb-org-mail-scan-state-changes' is non-nil, and
there is a gnus message link in the logbook, the above will be
disregarded in favor of replying to that link."
  :group 'gnorb-org
  :type '(or integer symbol))

(defcustom gnorb-org-mail-scan-state-changes 'first
  "This options influences how `gnorb-org-handle-mail' interprets
the current heading. If it is non-nil, the heading's state-change
notes will be given priority when looking for links to respond
to. If the state-change notes contain a gnus message link, that's
probably because `gnorb-gnus-message-trigger-todo' put it there,
and you're using the logbook drawer to keep track of an email
conversation. In that case, all other links will be disregared,
and a reply to the linked message will be started. Valid non-nil
values are 'first (only the most recent state-change note will be
scanned) and 'all (all notes will be scanned).

If this option is nil, the heading and its text will be scanned
as usual for links, subject to the value of
`gnorb-org-mail-scan-scope'."
  :group 'gnorb-org
  :type 'symbol)

(defcustom gnorb-org-mail-scan-function
  'gnorb-org-extract-mail-stuff
  "The function used to extract message links and email addresses
  from a heading and its text, for use in the
  `gnorb-org-handle-mail' and `gnorb-org-email-subtree'
  functions. It will be called at the heading of the current
  subtree. It's return value should be a list, containing two
  more lists: the first list is of links to gnus
  messages (currently only the first link will be acted upon).
  The second list is of strings suitable to be used in the To
  header of an outgoing email, ie \"Billy Bob Thornton
  <bbt@gmail.com>\".")

(defun gnorb-org-contact-link (rec)
  "Prompt for a BBDB record and insert a link to that record at
point.

There's really no reason to use this instead of regular old
`org-insert-link' with BBDB completion. But there might be in the
future!"
  ;; this needs to handle an active region.
  (interactive (list (gnorb-prompt-for-bbdb-record)))
  (let* ((name (bbdb-record-name rec))
	 (link (concat "bbdb:" (org-link-escape name))))
    (org-store-link-props :type "bbdb" :name name
			  :link link :description name)
    (if (called-interactively-p)
	(insert (format "[[%s][%s]]" link name))
      link)))

(defvar gnorb-org-window-conf nil
  "Save org-buffer window configuration here, for restoration
  after the mail is sent.")

(defun gnorb-org-restore-after-send ()
  "After an email is sent, clean up the gnus summary buffer, put
us back where we came from, and go through all the org ids that
might have been in the outgoing message's headers and call
`gnorb-org-do-restore-action' on each one."
  (when (eq major-mode 'gnus-summary-mode)
    (gnus-summary-exit nil t))
  (when (window-configuration-p gnorb-org-window-conf)
    (set-window-configuration gnorb-org-window-conf))
  (dolist (id gnorb-message-org-ids)
    (gnorb-trigger-todo-action nil id))
  ;; this is a little unnecessary, but it may save grief
  (setq gnorb-org-window-conf nil)
  (setq gnorb-gnus-sending-message-info nil)
  (setq gnorb-message-org-ids nil))

(defun gnorb-org-scan-state-notes ()
  "Look at the state-change notes of the heading and see if we
should be using links in those notes or not. If
`gnorb-org-mail-scan-state-changes' is set to 'first, only the
most recent state-change note is examined. Otherwise, each note
will be examined in reverse chronological order, and the first
message link found will be replied to."
  ;; gruesome
  (interactive)
  (let* ((org-log-into-drawer (org-log-into-drawer))
	 (drawer (cond ((stringp org-log-into-drawer)
			org-log-into-drawer)
		       (org-log-into-drawer "LOGBOOK")))
	 (search-dir (if org-log-states-order-reversed
			 're-search-forward
		       're-search-backward))
	 el type state-list)
    (save-excursion
      (forward-line)			; get off the heading
      (setq el (org-element-at-point)
	    type (org-element-type el))
      (while (memq type '(planning property-drawer))
	(org-forward-element)
	(setq el (org-element-at-point)
	      type (org-element-type el)))
      (cond
       (drawer
	(while (and (eq type 'drawer)
		    (not (equal drawer
				(org-element-property :drawer-name el))))
	  (org-forward-element)
	  (setq el (org-element-at-point)
		type (org-element-type el)))
	(when (equal drawer
		     (org-element-property :drawer-name el))
	  (forward-line)
	  (setq state-list (org-list-context))))
       (org-log-state-notes-insert-after-drawers
	(while (and (not (eq (point) (point-max)))
		    (eq type 'drawer))
	  (org-forward-element)
	  (setq el (org-element-at-point)
		type (org-element-type el)))
	(when (and (eq type 'plain-list)
		   (looking-at (concat
				(nth 2 (car (org-list-struct)))
				"State ")))
	  (setq state-list (org-list-context))))
       (t nil))
      (when state-list
	(let* ((origin (if org-log-states-order-reversed
			   (car state-list)
			 (second state-list)))
	       (item (org-in-item-p))
	       (struct (org-list-struct))
	       (prevs (org-list-prevs-alist struct))
	       (bound (if (eq gnorb-org-mail-scan-state-changes 'first)
			  (save-excursion
			    (goto-char
			     (if org-log-states-order-reversed
				 (org-list-get-first-item item struct prevs)
			       (org-list-get-last-item item struct prevs)))
			    (org-list-get-item-end item struct))
			(if org-log-states-order-reversed
			    (second state-list)
			  (car state-list)))))
	  (goto-char origin)
	  (when (funcall search-dir "\\[\\[\\(gnus:\\|mailto:\\|bbdb:\\)"
			 bound t)
	    (cons (min origin bound) (max origin bound))))))))

(defun gnorb-org-extract-mail-stuff ()
  "Extract mail-related information from the current heading. If
`gnorb-org-mail-scan-state-changes' is non-nil, it will be given
the chance to override the rest of the process and reply to a
link found in the state-change notes. Otherwise, the value of
`gnorb-org-mail-scan-scope' will determine how much of the
heading text will be scanned for message and mail links."
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (let* ((state-info
	      (and gnorb-org-mail-scan-state-changes
		   (gnorb-org-scan-state-notes)))
	     (start
	      (if state-info
		  (car state-info)
		;; get past drawers, and any non-drawer state-change
		;; list
		(forward-line)
		(while (and (not (eq (point) (point-max)))
			    (or
			     (memq (org-element-type (org-element-at-point))
				   '(planning drawer property-drawer))
			     (and org-log-state-notes-insert-after-drawers
				  (eq (org-element-type
				       (org-element-at-point)) 'plain-list)
				  (looking-at
				   (concat (nth 2 (car (org-list-struct)))
					   "State ")))))
		  (org-forward-element))
		(point)))
	     (end
	      (if state-info
		  (cdr state-info)
		(cond ((integerp gnorb-org-mail-scan-scope)
		       (forward-paragraph gnorb-org-mail-scan-scope))
		      ((eq gnorb-org-mail-scan-scope 'text)
		       (outline-next-heading))
		      ((eq gnorb-org-mail-scan-scope 'subtree)
		       (goto-char (point-max))))
		(point)))
	     message mails)
	(cl-labels
	    ((scan-for-links
	      (bound)
	      (unless (eq (point) bound)
		(while (re-search-forward org-any-link-re bound t)
		  (let ((addr (or (match-string-no-properties 2)
				  (match-string-no-properties 0))))
		    (cond
		     ((string-match "^<?gnus:" addr)
		      (push (substring addr (match-end 0)) message))
		     ((string-match "^<?mailto:" addr)
		      (push (substring addr (match-end 0)) mails))
		     ((string-match-p "^<?bbdb:" addr)
		      (with-current-buffer bbdb-buffer-name
			(let ((recs bbdb-records))
			  (org-open-link-from-string (concat "[[" addr "]]"))
			  (let ((mail (bbdb-mail-address (bbdb-current-record))))
			    (bbdb-display-records recs)
			    (push mail mails)))))))))))
	  (org-back-to-heading t)
	  (unless state-info
	    (scan-for-links (line-end-position)))
	  (goto-char start)
	  (scan-for-links end))
	(list message mails)))))

(defun gnorb-org-setup-message (&optional messages mails attachments text ids)
  "Common message setup routine for other gnorb-org commands.
MESSAGES is a list of gnus links pointing to messages -- we
currently only use the first of the list. MAILS is a list of
email address strings suitable for inserting in the To header.
ATTACHMENTS is a list of filenames to attach. TEXT is a string or
buffer, which is inserted in the message body. IDS is one or more
Org heading ids, associating the outgoing message with those
headings."
  (require 'gnorb-gnus)
  (if (not messages)
      ; either compose new message...
      (compose-mail (mapconcat 'identity mails ", "))
    ; ...or follow link and start reply
    (condition-case nil
	(progn
	  (org-gnus-open (org-link-unescape (car messages)))
	  (call-interactively
	   'gnus-summary-wide-reply-with-original)
	  ;; add MAILS to message To header
	  (when mails
	    (message-goto-to)
	    (insert ", ")
	    (insert (mapconcat 'identity mails ", "))))
      (error (message "Couldn't open linked message"))))
  ;; return us after message is sent
  (add-to-list 'message-exit-actions
	       'gnorb-org-restore-after-send t)
  ; attach ATTACHMENTS
  (map-y-or-n-p
   (lambda (a) (format "Attach %s to outgoing message? "
		       (file-name-nondirectory a)))
   (lambda (a)
     (mml-attach-file a (mm-default-file-encoding a)
      nil "attachment"))
   attachments
   '("file" "files" "attach"))
  ;; insert text, if any
  (when text
    (message-goto-body)
    (insert"\n")
    (if (bufferp text)
	(insert-buffer text)
      (insert text)))
  ;; insert org ids, if any
  (when ids
    (unless (listp ids)
      (setq ids (list ids)))
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(dolist (i ids)
	  (goto-char (point-at-bol))
	  (open-line 1)
	  ;; this function hardly does anything
	  (message-insert-header
	   (intern gnorb-mail-header) i)))))
  ; put point somewhere reasonable
  (if (or mails messages)
      (message-goto-body)
    (message-goto-to))
  (run-hooks 'gnorb-org-after-message-setup-hook))

(defun gnorb-org-attachment-list ()
  "Get a list of files (absolute filenames) attached to the
current heading."
  (when (featurep 'org-attach)
    (let* ((attach-dir (org-attach-dir t))
	   (files
	    (mapcar
	     (lambda (f)
	       (expand-file-name f attach-dir))
	     (org-attach-file-list attach-dir))))
      files)))

(defun gnorb-org-handle-mail ()
  "Handle current headline as a mail TODO."
  (interactive)
  (setq gnorb-org-window-conf (current-window-configuration))
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-check-type t 'agenda 'timeline 'todo 'tags)
    (org-agenda-check-no-diary)
    (let* ((marker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   (pos (marker-position marker)))
      (switch-to-buffer buffer)
      (widen)
      (goto-char pos))) 
  (unless (org-back-to-heading t)
    (error "Not in an org item"))
  (let ((mail-stuff (funcall gnorb-org-mail-scan-function))
	(attachments (gnorb-org-attachment-list))
	(org-id (org-id-get-create)))
    (gnorb-org-setup-message
     (first mail-stuff) (second mail-stuff)
     attachments nil org-id)))

(defun gnorb-org-find-visit-candidates (ids)
  "For all message-ids in IDS (which should be a list of
Message-ID strings, with angle brackets), produce a list of Org
ids (and ol-paths) for headings that contain one of those id
values in their `gnorb-org-org-msg-id-key' property."
  ;; org-id actually uses an external file to make this whole process
  ;; faster, but we don't really need that kind of efficiency, I don't
  ;; think. Visiting `org-agenda-files' and collecting property values
  ;; should be okay. Speedups later, if and when needed. Right now
  ;; this only happens on an interactive function call by the user, so
  ;; a little pause is acceptable. Later we might try to add it to a
  ;; notice-message type of hook, in which case I'll think about some
  ;; sort of primitive caching. Since all we need is a mapping between
  ;; Org ids and lists of message ids, maybe a hash table with Org id
  ;; keys. It would need to be refreshed whenever the
  ;; `gnorb-org-msg-id-key' was set. Deletions we could ignore: visit
  ;; the ID, and if it doesn't exist or doesn't have the msg-id-key
  ;; property, then refresh the cache and start over.

  ;; Or see how org-id does it -- since the whole things relies on
  ;; org-id, we could maybe just refresh our table when org-id
  ;; refreshes.

  ;; use `org-format-outline-path' to show the path at the other end.

  ;; Probably I should have this function return a value that can be
  ;; pushed to the front of org-refile-history, so that it's just
  ;; offered as a default. Then things like
  ;; `org-refile-goto-last-stored' and all that will work without my
  ;; having to write new equivalents.
  (let (ret-val)
    (setq ret-val
	  (append
	   (org-map-entries
	    (lambda ()
	      (catch 'done
		(dolist (id ids)
		  (when
		      (org-entry-member-in-multivalued-property
		       (point) gnorb-org-msg-id-key id)
		    (throw 'done
			   (list (org-id-get-create)
				 (append
				  (org-get-outline-path)
				  (list (org-get-heading nil t)))))))))
	    nil ;; allow customize here, default to
		;; `gnorb-org-mail-todos', but maybe provide a
		;; separate option.
	    'agenda 'archive 'comment)
	   ret-val))
    (setq ret-val (delete-dups
		   (delq nil ret-val)))))

;;; Email subtree

(defcustom gnorb-org-email-subtree-text-parameters nil
  "A plist of export parameters corresponding to the EXT-PLIST
  argument to the export functions, for use when exporting to
  text."
  :group 'gnorb-org
  :type 'boolean)

(defcustom gnorb-org-email-subtree-file-parameters nil
  "A plist of export parameters corresponding to the EXT-PLIST
  argument to the export functions, for use when exporting to a
  file."
  :group 'gnorb-org
  :type 'boolean)

(defcustom gnorb-org-email-subtree-text-options '(nil t nil t)
  "A list of ts and nils corresponding to Org's export options,
to be used when exporting to text. The options, in order, are
async, subtreep, visible-only, and body-only."
  :group 'gnorb-org
  :type 'list)

(defcustom gnorb-org-email-subtree-file-options '(nil t nil nil)
  "A list of ts and nils corresponding to Org's export options,
to be used when exporting to a file. The options, in order, are
async, subtreep, visible-only, and body-only."
  :group 'gnorb-org
  :type 'list)

(defcustom gnorb-org-export-extensions
  '((latex ".tex")
    (ascii ".txt")
    (html ".html")
    (org ".org")
    (icalendar ".ics")
    (man ".man")
    (md ".md")
    (odt ".odt") ; not really, though
    (texinfo ".texi")
    (beamer ".tex"))
  "Correspondence between export backends and their
respective (usual) file extensions. Ugly way to do it, but what
the hey..."
  :group 'gnorb-org)

(defun gnorb-org-email-subtree ()
  "Call on a subtree to export it either to a text string or a file,
then compose a mail message either with the exported text
inserted into the message body, or the exported file attached to
the message.

Export options default to the following: When exporting to a
buffer: async = nil, subtreep = t, visible-only = nil, body-only
= t. Options are the same for files, except body-only is set to
nil. Customize `gnorb-org-email-subtree-text-options' and
`gnorb-org-email-subtree-file-options', respectively.

Customize `gnorb-org-email-subtree-parameters' to your preferred
default set of parameters."
  ;; I sure would have liked to use the built-in dispatch ui, but it's
  ;; got too much hard-coded stuff.
  (interactive)
  (org-back-to-heading t)
  (let* ((backend-string
	  (org-completing-read
	   "Export backend: "
	   (mapcar (lambda (b)
		     (symbol-name (org-export-backend-name b)))
		   org-export--registered-backends) nil t))
	 (backend-symbol (intern backend-string))
	 (f-or-t (org-completing-read "Export as file or text? "
				      '("file" "text") nil t))
	 (org-export-show-temporary-export-buffer nil)
	 (opts (if (equal f-or-t "text")
		   gnorb-org-email-subtree-text-options
		 gnorb-org-email-subtree-file-options))
	 (result
	  (if (equal f-or-t "text")
	      (apply 'org-export-to-buffer
		     `(,backend-symbol
		       "*Gnorb Export*"
		       ,@opts
		       ,gnorb-org-email-subtree-text-parameters))
	    (apply 'org-export-to-file
		   `(,backend-symbol
		     ,(org-export-output-file-name
		       (second (assoc backend-symbol gnorb-org-export-extensions))
		       t gnorb-tmp-dir)
		     ,@opts
		     ,gnorb-org-email-subtree-file-parameters))))
	 (mail-stuff (funcall gnorb-org-mail-scan-function))
	 (attachments (gnorb-org-attachment-list))
	 (org-id (org-id-get-create))
	 text)
    ;; this should just go into a call to `org-handle-mail', passing
    ;; the results of the export as an argument
    (setq gnorb-org-window-conf (current-window-configuration))
    (if (bufferp result)
	(setq text result)
      (push result attachments))
    (gnorb-org-setup-message
     (first mail-stuff) (second mail-stuff)
     attachments text org-id)))

(defcustom gnorb-org-capture-collect-link-p t
  "Should the capture process store a link to the gnus message or
  BBDB record under point, even if it's not part of the template?
  You'll probably end up needing it, anyway."
  :group 'gnorb-org)

(defun gnorb-org-capture-collect-link ()
  (when gnorb-org-capture-collect-link-p
    (let ((buf (org-capture-get :original-buffer)))
      (when buf
	(with-current-buffer buf
	  (when (memq major-mode '(gnus-summary-mode
				   gnus-article-mode
				   bbdb-mode))
	    (call-interactively 'org-store-link)))))))

(add-hook 'org-capture-mode-hook 'gnorb-org-capture-collect-link)

;;; Agenda/BBDB popup stuff

(defcustom gnorb-org-agenda-popup-bbdb nil
  "Should Agenda tags search pop up a BBDB buffer with matching
  records?

Records are considered matching if they have an `org-tags' field
matching the current Agenda search. The name of that field can be
customized with `gnorb-bbdb-org-tag-field'."
  :group 'gnorb-org)

(defcustom gnorb-org-bbdb-popup-layout 'pop-up-multi-line
  "Default BBDB buffer layout for automatic Org Agenda display."
  :group 'gnorb-org
  :type '(choice (const one-line)
		 (const multi-line)
		 (const full-multi-line)
		 (symbol)))

(defun gnorb-org-popup-bbdb (&optional str)
  "In an `org-tags-view' Agenda buffer, pop up a BBDB buffer
showing records whose `org-tags' field matches the current tags
search."
  ;; I was hoping to use `org-make-tags-matcher' directly, then snag
  ;; the tagmatcher from the resulting value, but there doesn't seem
  ;; to be a reliable way of only getting the tag-related returns. But
  ;; I'd still like to use that function. So an ugly hack to first
  ;; remove non-tag contents from the query string, and then make a
  ;; new call to `org-make-tags-matcher'.
  (interactive)
  (require 'gnorb-bbdb)
  (let (recs)
    (cond ((and
	    (and (eq major-mode 'org-agenda-mode)
		 (eq org-agenda-type 'tags))
	    (or (called-interactively-p)
		gnorb-org-agenda-popup-bbdb))
	   (let ((todo-only nil)
		 (str (or str org-agenda-query-string))
		 (re "^&?\\([-+:]\\)?\\({[^}]+}\\|LEVEL\\([<=>]\\{1,2\\}\\)\\([0-9]+\\)\\|\\(\\(?:[[:alnum:]_]+\\(?:\\\\-\\)*\\)+\\)\\([<>=]\\{1,2\\}\\)\\({[^}]+}\\|\"[^\"]*\"\\|-?[.0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)\\|[[:alnum:]_@#%]+\\)")
		 or-terms term rest out-or acc tag-clause)
	     (setq or-terms (org-split-string str "|"))
	     (while (setq term (pop or-terms))
	       (setq acc nil)
	       (while (string-match re term)
		 (setq rest (substring term (match-end 0)))
		 (let ((sub-term (match-string 0 term)))
		   (unless (save-match-data ; this isn't a tag, don't want it
			     (string-match "\\([<>=]\\)" sub-term))
		     (push sub-term acc))
		   (setq term rest)))
	       (push (mapconcat 'identity (nreverse acc) "") out-or))
	     (setq str (mapconcat 'identity (nreverse out-or) "|"))
	     (setq tag-clause (cdr (org-make-tags-matcher str)))
	     (setq recs
		   (remove-if-not
		    (lambda (r)
		      (let ((rec-tags (bbdb-record-xfield
				       r gnorb-bbdb-org-tag-field)))
			(and rec-tags
			     (let ((tags-list (org-split-string rec-tags ":"))
				   (case-fold-search t)
				   (org-trust-scanner-tags t))
			       (eval tag-clause)))))
		    (bbdb-records)))))
	  ((eq major-mode 'org-mode)
	   (save-excursion
	     (org-back-to-heading)
	     (while (re-search-forward
		     org-bracket-link-analytic-regexp (line-end-position) t)
	       (when (string-match-p "bbdb" (match-string 2))
		 (let* ((desc (match-string 5))
			(rec (bbdb-search (bbdb-records) desc desc desc)))
		   (setq recs (append recs rec))))))))
    (if recs
	(bbdb-display-records
	 recs gnorb-org-bbdb-popup-layout)
      (when (get-buffer-window bbdb-buffer-name)
	(quit-window nil
		     (get-buffer-window bbdb-buffer-name)))
      (when (called-interactively-p)
	(message "No relevant BBDB records")))))

(add-hook 'org-agenda-finalize-hook 'gnorb-org-popup-bbdb)

(provide 'gnorb-org)
;;; gnorb-org.el ends here
