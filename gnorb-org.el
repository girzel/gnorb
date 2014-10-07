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

(defcustom gnorb-org-after-message-setup-hook nil
  "Hook run in a message buffer after setting up the message from
  `gnorb-org-handle-mail' or `gnorb-org-email-subtree'."
  :group 'gnorb-org
  :type 'hook)

(defcustom gnorb-org-trigger-actions
  '(("todo state" . todo)
    ("take note" . note)
    ("don't associate" . no-associate)
    ("only associate" . associate)
;    ("capture to child" . cap-child)
;    ("capture to sibling" . cap-sib)
)
  "List of potential actions that can be taken on headings.

When triggering an Org heading after receiving or sending a
message, this option lists the possible actions to take. Built-in
actions include:

todo state: Associate the message, and change TODO state.
take note: Associate the message, and take a note.
don't associate: Do nothing at all, don't connect the message and TODO.
only associate: Associate the message with this heading, do nothing else.
capture to child: [not yet implemented] Associate this message with a new child heading.
capture to sibling: [not yet implemented] Associate this message with a new sibling heading.

You can reorder this list or remove items as suits your workflow.
The two \"capture\" options will use the value of
`gnorb-gnus-new-todo-capture-key' to find the appropriate
template.

You can also add custom actions to the list. Actions should be a
cons of a string tag and a symbol indicating a custom function.
This function will be called on the heading in question, and
passed a plist containing information about the message from
which we're triggering."
  :group 'gnorb-org
  :type 'list)

(defcustom gnorb-org-msg-id-key "GNORB_MSG_ID"
  "The name of the org property used to store the Message-IDs
  from relevant messages. This is no longer used, and will be
  removed soon."
  :group 'gnorb-org
  :type 'string)

(defcustom gnorb-org-mail-scan-scope 2
  "Number of paragraphs to scan for mail-related links.

When handling a TODO heading with `gnorb-org-handle-mail', Gnorb
will typically reply to the most recent message associated with
this heading. If there are no such messages, or message tracking
is disabled entirely, or `gnorb-org-handle-mail' has been called
with a prefix arg, the heading and body text of the subtree under
point will instead be scanned for gnus:, mailto:, and bbdb:
links. This option controls how many paragraphs of body text to
scan. Set to 0 to only look in the heading.")

(make-obsolete-variable
 'gnorb-org-mail-scan-strategies
 "This variable has been superseded by `gnorb-org-trigger-actions'"
 "September 12, 2014" 'set)

(make-obsolete-variable
 'gnorb-org-mail-scan-state-changes
 "This variable has been superseded by `gnorb-org-trigger-actions'"
 "September 12, 2014" 'set)

(make-obsolete-variable
 'gnorb-org-mail-scan-function
 "This variable has been superseded by `gnorb-org-trigger-actions'"
 "September 12, 2014" 'set)

(defcustom gnorb-org-find-candidates-match nil
  "When scanning all org files for heading related to an incoming
message, this option will limit which headings will be offered as
target candidates. Specifically it will be used as the second
argument to `org-map-entries', and syntax is the same as that
used in an agenda tags view."
  :group 'gnorb-org
  :type 'symbol)

;;;###autoload
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
    (if (called-interactively-p 'any)
	(insert (format "[[%s][%s]]" link name))
      link)))

(defun gnorb-org-restore-after-send ()
  "After an email is sent, clean up the gnus summary buffer, put
us back where we came from, and go through all the org ids that
might have been in the outgoing message's headers and call
`gnorb-trigger-todo-action' on each one."
  (delete-other-windows)
  (dolist (id gnorb-message-org-ids)
    (org-id-goto id)
    (org-reveal)
    (gnorb-trigger-todo-action nil id))
  ;; this is a little unnecessary, but it may save grief
  (setq gnorb-gnus-message-info nil)
  (setq gnorb-message-org-ids nil))

(defun gnorb-org-extract-links (&optional arg region)
  "See if there are viable links in the subtree under point."
  ;; We're not currently using the arg. What could we do with it?
  (let (strings)
    ;; If the region was active, only use the region
    (if region
	(push (buffer-substring (car region) (cdr region))
	      strings)
      ;; Otherwise collect the heading text, and all the paragraph
      ;; text.
      (save-restriction
	(org-narrow-to-subtree)
	(let ((head (org-element-at-point))
	      (tree (org-element-parse-buffer)))
	  (push (org-element-property
		 :raw-value
		 head)
		strings)
	  (org-element-map tree 'paragraph
	    (lambda (p)
	      (push (org-element-interpret-data p)
		    strings))
	    nil nil 'drawer))))
    (when strings
      ;; Limit number of paragraphs based on
      ;; `gnorb-org-mail-scan-scope'
      (setq strings
	    (cond ((eq gnorb-org-mail-scan-scope 'all)
		   strings)
		  ((numberp gnorb-org-mail-scan-scope)
		   (delq nil
			 (subseq
			  strings 0 (1+ gnorb-org-mail-scan-scope))))
		  ;; We could provide more options here. 'tree vs
		  ;; 'subtree, for instance.
		  (t
		   strings)))
      (with-temp-buffer
	(dolist (s strings)
	  (insert s)
	  (insert "\n"))
	(goto-char (point-min))
	(gnorb-scan-links (point-max) 'gnus 'mail 'bbdb)))))

(defun gnorb-org-extract-mail-stuff (&optional arg region)
  "Decide how to hande the Org heading under point as an email task.

See the docstring of `gnorb-org-handle-mail' for details."
  (if (or (not gnorb-tracking-enabled)
	  region)
      (gnorb-org-extract-links arg region)
    ;; Get all the messages associated with the IDS in this subtree.
    (let ((assoc-msg-ids
	   (delete-dups
	    (cl-mapcan
	     (lambda (id)
	       (gnorb-registry-org-id-search id))
	     (gnorb-collect-ids)))))
      (gnorb-org-extract-mail-tracking assoc-msg-ids arg region))))

(defun gnorb-org-extract-mail-tracking (assoc-msg-ids &optional arg region)

  (let* ((all-links (gnorb-org-extract-links nil region))
	 ;; The latest (by the creation-time registry key) of all the
	 ;; tracked messages that were not sent by our user.
	 (latest-msg-id
	  (when assoc-msg-ids
	    (car
	     (sort
	      (remove-if
	       (lambda (m)
		 (let ((from (car (gnus-registry-get-id-key m 'sender))))
		   (or (null from)
		       (string-match-p
			user-mail-address from)
		       (string-match-p
			message-alternative-emails from))))
	       assoc-msg-ids)
	      (lambda (r l)
		(time-less-p
		 (car (gnus-registry-get-id-key l 'creation-time))
		 (car (gnus-registry-get-id-key r 'creation-time)))))))))
    (cond
     ;; If there are no tracked messages, or the user has specifically
     ;; requested we ignore them with the prefix arg, just return the
     ;; found links in the subtree.
     ((or arg
	  (null latest-msg-id))
      all-links)
     ;; Otherwise ignore the other links in the subtree, and return
     ;; the latest message.
     (latest-msg-id
      `(:gnus ,(list (gnorb-msg-id-to-link latest-msg-id)))))))

(defun gnorb-org-setup-message
    (&optional messages mails from cc bcc attachments text ids)
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
      ;; Either compose new message...
      (compose-mail (mapconcat 'identity mails ", "))
    ;; ...or follow link and start reply.
    (condition-case err
	(let ((ret-val (org-gnus-open (org-link-unescape (car messages)))))
	  ;; We failed to open the link (probably), ret-val would be
	  ;; t otherwise
	  (when (stringp ret-val)
	    (error ret-val))
	  (call-interactively
	   'gnus-summary-wide-reply-with-original)
	  ;; Add MAILS to message To header.
	  (when mails
	    (message-goto-to)
	    (insert ", ")
	    (insert (mapconcat 'identity mails ", "))))
      (error (when (and (window-configuration-p gnorb-window-conf)
			gnorb-return-marker)
	       (set-window-configuration gnorb-window-conf)
	       (goto-char gnorb-return-marker))
	     (signal (car err) (cdr err)))))
  ;; Return us after message is sent.
  (add-to-list 'message-exit-actions
	       'gnorb-org-restore-after-send t)
  ;; Set headers from MAIL_* properties (from, cc, and bcc).
  (cl-flet ((sh (h)
		(when (cdr h)
		  (funcall (intern (format "message-goto-%s" (car h))))
		  (let ((message-beginning-of-line t)
			(show-trailing-whitespace t))
		    (message-beginning-of-line)
		    (unless (bolp)
		      (kill-line))
		    (insert (cdr h))))))
    (dolist (h `((from . ,from) (cc . ,cc) (bcc . ,bcc)))
      (sh h)))
  ;; attach ATTACHMENTS
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
	(insert-buffer-substring text)
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
  ;; put point somewhere reasonable
  (if (or mails messages)
      (if (not messages)
	  (message-goto-subject)
       (message-goto-body))
    (message-goto-to))
  (run-hooks 'gnorb-org-after-message-setup-hook))

(defun gnorb-org-attachment-list (&optional id)
  "Get a list of files (absolute filenames) attached to the
current heading, or the heading indicated by optional argument ID."
  (when (featurep 'org-attach)
    (let* ((attach-dir (save-excursion
			 (when id
			   (org-id-goto id))
			 (org-attach-dir t)))
	   (files
	    (mapcar
	     (lambda (f)
	       (expand-file-name f attach-dir))
	     (org-attach-file-list attach-dir))))
      files)))

;;;###autoload
(defun gnorb-org-handle-mail (&optional arg text file)
  "Handle current headline as a mail TODO.

How this function behaves depends on whether you're using Gnorb
for email tracking, also on the prefix arg, and on the active
region.

If tracking is enabled and there is no prefix arg, Gnorb will
begin a reply to the newest associated message that wasn't sent
by the user -- ie, the Sender header doesn't match
`user-mail-address' or `message-alternative-emails'.

If tracking is enabled and there is a prefix arg, ignore the
tracked messages and instead scan the subtree for mail-related
links. This means links prefixed with gnus:, mailto:, or bbdb:.
See `gnorb-org-mail-scan-scope' to limit the scope of this scan.
Do something appropriate with the resulting links.

With a double prefix arg, ignore all tracked messages and all
links, and compose a blank new message.

If tracking is enabled and you want to reply to a
specific (earlier) message in the tracking history, use
`gnorb-org-view' to open an nnir *Summary* buffer containing all
the messages, and reply to the one you want. Your reply will be
automatically tracked, as well.

If tracking is not enabled and you want to use a specific link in
the subtree as a basis for the email action, then put the region
around that link before you call this message."
  (interactive "P")
  (setq gnorb-window-conf (current-window-configuration))
  (move-marker gnorb-return-marker (point))
  (when (eq major-mode 'org-agenda-mode)
    ;; If this is all the different types, we could skip the check.
    (org-agenda-check-type t 'agenda 'timeline 'todo 'tags 'search)
    (org-agenda-check-no-diary)
    (let* ((marker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   (pos (marker-position marker)))
      (switch-to-buffer buffer)
      (widen)
      (goto-char pos)))
  (let ((region
	 (when (use-region-p)
	   (cons (region-beginning) (region-end)))))
    (deactivate-mark)
    (save-excursion
      (unless (org-back-to-heading t)
	(error "Not in an org item"))
      (cl-flet ((mp (p) (org-entry-get (point) p t)))
	;; Double prefix means ignore everything and compose a blank
	;; mail.
	(let* ((links (unless (equal arg '(16))
			(gnorb-org-extract-mail-stuff arg region)))
	       (attachments (gnorb-org-attachment-list))
	       (from (mp "MAIL_FROM"))
	       (cc (mp "MAIL_CC"))
	       (bcc (mp "MAIL_BCC"))
	       (org-id (org-id-get-create))
	       (recs (plist-get links :bbdb))
	       (message-mode-hook (copy-sequence message-mode-hook))
	       mails)
	  (when file
	    (cons file attachments))
	  (when recs
	    (setq recs
		  (delq nil
			(mapcar
			 (lambda (r)
			   (car (bbdb-message-search
				 (org-link-unescape r)
				 nil)))
			 recs))))
	  (when recs
	    (dolist (r recs)
	      (push (bbdb-mail-address r) mails)))
	  (when (and recs
		     gnorb-bbdb-posting-styles)
	    (add-hook 'message-mode-hook
		      (lambda ()
			(gnorb-bbdb-configure-posting-styles (cdr recs))
			(gnorb-bbdb-configure-posting-styles (list (car recs))))))
	  (gnorb-org-setup-message
	   (plist-get links :gnus)
	   (append mails (plist-get links :mail))
	   from cc bcc
	   attachments text org-id))))))

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

;;;###autoload
(defun gnorb-org-email-subtree (&optional arg)
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
  (interactive "P")
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
	 text file)
    (setq gnorb-window-conf (current-window-configuration))
    (move-marker gnorb-return-marker (point))
    (if (bufferp result)
	(setq text result)
      (setq file result))
    (gnorb-org-handle-mail arg text file)))

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

;;;###autoload
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
	    (or (called-interactively-p 'any)
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
	     (unless (equal str "")
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
		      (bbdb-records))))))
	  ((eq major-mode 'org-mode)
	   (save-excursion
	     (org-back-to-heading)
	     (let ((bound (org-element-property
			   :end (org-element-at-point)))
		   desc rec)
	       (while (re-search-forward
		       org-bracket-link-analytic-regexp bound t)
		 (when (string-match-p "bbdb" (match-string 2))
		   (setq desc (match-string 5)
			 rec (bbdb-search (bbdb-records) desc desc desc)
			 recs (append recs rec))))))))
    (if recs
	(bbdb-display-records
	 recs gnorb-org-bbdb-popup-layout)
      (when (get-buffer-window bbdb-buffer-name)
	(quit-window nil
		     (get-buffer-window bbdb-buffer-name)))
      (when (called-interactively-p 'any)
	(message "No relevant BBDB records")))))

(if (featurep 'gnorb-bbdb)
    (add-hook 'org-agenda-finalize-hook 'gnorb-org-popup-bbdb))

;;; Groups from the gnorb gnus server backend

;;;###autoload
(defun gnorb-org-view ()
  "Search the subtree at point for links to gnus messages, and
then show them in an ephemeral group, in gnus.

This won't work unless you've added a \"nngnorb\" server to
your gnus select methods."
  ;; this should also work on the active region, if there is one.
  (interactive)
  (setq gnorb-window-conf (current-window-configuration))
  (move-marker gnorb-return-marker (point))
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-check-type t 'agenda 'timeline 'todo 'tags)
    (org-agenda-check-no-diary)
    (let* ((marker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   (pos (marker-position marker)))
      (switch-to-buffer buffer)
      (goto-char pos)
      (org-reveal)))
  (let (id)
    (save-excursion
      (org-back-to-heading)
      (setq id (concat "id+" (org-id-get-create))))
    (gnorb-gnus-search-messages
     id
     `(when (and (window-configuration-p gnorb-window-conf)
		 gnorb-return-marker)
	(set-window-configuration gnorb-window-conf)
	(goto-char gnorb-return-marker)))))

(provide 'gnorb-org)
;;; gnorb-org.el ends here
