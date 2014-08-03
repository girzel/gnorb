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

(defcustom gnorb-org-mail-todos nil
  "TODO keywords that are considered mail related -- functions in
this library may, after completion, call org-(agenda-)todo on
relevant headings with these keywords. Set to nil to call
org-todo regardless of TODO type."
  ;; allowable values should also include 'todo and 'not-done
  :group 'gnorb-org
  :type 'list)

(defcustom gnorb-org-after-message-setup-hook nil
  "Hook run in a message buffer after setting up the message from
  `gnorb-org-handle-mail' or `gnorb-org-email-subtree'."
  :group 'gnorb-org
  :type 'hook)

(defcustom gnorb-org-msg-id-key "GNORB_MSG_ID"
  "The name of the org property used to store the Message-IDs
  from relevant messages."
  :group 'gnorb-org
  :type 'string)

(defcustom gnorb-org-mail-scan-strategies
  '(((:type state :scope first-link) 1)
    (nil text)
    offer)
  "This option controls how `gnorb-org-handle-mail' scans the
subtree under point for links, and what it does with the links it
finds. It is a list of up to three elements, representing three
different scanning strategies. The first is used when calling the
function with no prefix arg; the second is used with a single
\\[universal-argument], and the third with a double
\\[universal-argument]. You can thus prepare different scanning
strategies in advance, and choose whichever is appropriate.

Each \"strategy\" will usually be a list of two items. The first
item determines how the heading's state-change notes are scanned,
the second item governs how the heading's body text is scanned.
If the scan of the state notes produces usable links, the second
item will be disregarded -- the rest of the heading won't be
scanned at all. This is because, if you're responding to message
links in the state-notes, they probably represent later messages
in an ongoing conversation. If you want a particular strategy to
always skip the state notes, just set the first item to nil.

The state specification is an alist with two possible keys: :type
and :scope. The :type key can be set to either 'state or 'note,
which means the scanning process will only consider logdrawer
items of one sort or the other. Setting the key to anything
lese (or leaving it out) means both state-change notes and
regular notes will be scanned. The :scope key controls how many
items will be scanned. Set to a positive integer to scan that
many items. The symbol 'first is a synonym for 1. The symbol
'first-link means scan each state note until one containing links
is found, and use only links from that item. The symbol 'all
means scan all state notes.

The second specification is used when the state-notes scan
produces no results, or was skipped with a nil specification.
This second item can be 0, meaning only scan the text of the
heading itself; a positive integer, meaning scan that many
paragraphs of body text; the symbol 'text, meaning scan the
entire body text; or the symbol 'subtree, meaning scan the
heading's text and all its subtrees.

Instead of a two-value specification, a strategy can just be a
single symbol: 'all will scan both the state notes and the body
text, and 'offer will collect all the links in the entire subtree
and pop up a buffer allowing the user to choose which links to
act on [this is a lie, 'offer hasn't been implemented yet].

Lastly, any of the elements can be a symbol representing a custom
function. When placed in the state-log or body-text
specifications, the function will be called in a temporary buffer
containing only the text of the state-log drawer, or the body
text, respectively. If the entire strategy is replaced by a
function name, that function will be called with point at the
beginning of the heading. Custom functions can use the internal
function `gnorb-scan-links' to return the appropriate alist of
links.

If `gnorb-org-handle-mail' is called while the region is active,
this variable will be disregarded entirely, and only the active
region will be scanned for links. If you call
`gnorb-org-handle-mail' with a prefix argument while the region
is active, it will look for links everywhere _but_ the active
region.

If all that sounds confusing, consider the default value:

'(((:type state :scope first-link) 1)
  (nil text)
  offer)

With no prefix arg, `gnorb-org-handle-mail' will look into the
logbook, and look at each state log item (skipping regular notes)
until it finds a state log with links in it, then operate on all
the links in that log item. If it finds nothing in the drawer, it
will scan the text of the heading, and the first paragraph of
body text.

With one prefix arg, it will always ignore the state-change
notes, instead scanning the heading and the entirety of its body
text.

With two prefix args, it will simply offer all the links in the
subtree for selection.")

(make-obsolete-variable
 'gnorb-org-mail-scan-scope
 "This variable has been superseded by `gnorb-org-mail-scan-strategies'"
 "June 7, 2014" 'set)

(make-obsolete-variable
 'gnorb-org-mail-scan-state-changes
 "This variable has been superseded by `gnorb-org-mail-scan-strategies'"
 "June 7, 2014" 'set)

(make-obsolete-variable
 'gnorb-org-mail-scan-function
 "This variable has been superseded by `gnorb-org-mail-scan-strategies'"
 "June 7, 2014" 'set)

(defcustom gnorb-org-find-candidates-match nil
  "When scanning all org files for heading related to an incoming
message, this option will limit which headings will be offered as
target candidates. Specifically it will be used as the second
argument to `org-map-entries', and syntax is the same as that
used in an agenda tags view.

For instance, to limit candidates to headings with TODOs in
`gnorb-org-mail-todos', you might use this:

(mapconcat (lambda (s) (format \"+TODO=\\\"%s\\\"\" s)) gnorb-org-mail-todos \"|\")"
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

(defun gnorb-org-extract-mail-stuff (strategy &optional region)
  "Extract mail-related information from the current heading. How
the heading is scanned depends on the value of
`gnorb-org-mail-scan-strategies' -- STRATEGY represents an
element chosen from that variable. If BOUNDS is non-nil, it
should represent point and mark of the active region, and means
STRATEGY will be disregarded."
  (save-restriction
    (org-narrow-to-subtree)
    ;; first collect all the relevant bits of the subtree
    (let* ((parsed (org-element-parse-buffer))
	   (headline
	    (org-element-map parsed 'headline 'identity nil t))
	   (head-text (org-element-property :raw-value headline))
	   (state-log
	    (org-element-map parsed 'plain-list
	      (lambda (l)
		(when (org-element-map l 'paragraph
			;; kludge to tell a state-log drawer list from
			;; a regular old list.
			(lambda (p)
			  (string-match-p
			   "\\(State \"\\|Note taken on\\)"
			   (car (org-element-contents p)))) nil t)
		  l)) nil t))
	   (pars
	    (org-element-map parsed 'paragraph
	      (lambda (p)
		(buffer-substring
		 (org-element-property :contents-begin p)
		 (org-element-property :contents-end p)))
	      nil nil 'drawer))
	   state-strategy text-strategy search-func
	   strings state-success all-links)
      (when (listp strategy)
	(setq state-strategy (car strategy)
	      text-strategy (nth 1 strategy)))
      ;; Order of precedence is: active region beats custom function
      ;; beats all-or-offer beats state-logs beats general text
      ;; scan. First we check everything up to all-or-offer.
      (unless
	  (cond
	   ((and region (eq 'reverse-region strategy))
	    (setq strings
		  ;; sure hope the region is contained within the
		  ;; headline!
		  (list
		   (buffer-substring
		    (point-min)
		    (car region))
		   (buffer-substring
		    (cdr region)
		    (point-max)))))
	   (region
	    (push (buffer-substring (car region) (cdr region))
		  strings))
	   ((and (symbolp strategy)
		 (fboundp strategy))
	    ;; user is responsible for finding links
	    (setq strings
		  (list
		   (buffer-substring
		    (point-min)
		    (point-max))))
	    (setq search-func strategy))
	   ((eq strategy 'all)
	    (setq strings
		  (list
		   (buffer-substring
		    (point-min)
		    (point-max)))))
	   ((eq strategy 'offer)
	    (user-error "Don't use 'offer, it's not done yet")))
	;; The above produced nothing, so try first the
	;; state-logs, then the body text
	(when (and state-log state-strategy)
	  (cond
	   ((and (symbolp state-strategy)
		 (fboundp state-strategy)
		 (setq all-links
		       (gnorb-org-find-links
			(buffer-substring
			 (org-element-property :begin state-log)
			 (org-element-property :end state-log))
			state-strategy))
		 (setq state-success t)))
	   ((listp state-strategy)
	    (when (setq all-links
			(gnorb-org-scan-log-notes
			 state-log state-strategy))
	      (setq state-success t)))
	   (t
	    (and (setq
		  all-links
		  (gnorb-org-find-links
		   (buffer-substring
		    (org-element-property :begin state-log)
		    (org-element-property :end state-log))))
		 (setq state-success t)))))
	;; at last, we get to check the plain old text
	(when (and (not state-success) text-strategy)
	  (cond
	   ((and (symbolp text-strategy)
		 (fboundp text-strategy))
	    (setq strings
		  (cons
		   head-text
		   pars))
	    (setq search-func text-strategy))
	   ((eq 'text text-strategy)
	    (setq strings
		  (cons
		   head-text
		   pars)))
	   ((eq 'subtree text-strategy)
	    (setq strings
		  (list
		   head-text
		   (buffer-substring-no-properties
		    (org-element-map headline 'paragraph
		      (lambda (p)
			(org-element-property :begin p))
		      nil t 'drawer)
		    (point-max)))))
	   ((integerp text-strategy)
	    (setq strings
		  (cons
		   head-text
		   (subseq pars 0 text-strategy)))))))
      ;; return the links if we've got them, or find them in strings
      (setq strings (delq nil strings))
      (when (and strings (not all-links))
	(setq all-links (gnorb-org-find-links strings search-func)))
      all-links)))

(defun gnorb-org-scan-log-notes (state-log strategy)
  ;; `gnorb-org-extract-mail-stuff' was way too long already

  ;; I've had a hell of a time just figuring out how to get the
  ;; complete paragraph text out of a parsed paragraph.
  (let ((type (plist-get strategy :type))
	(scope (plist-get strategy :scope))
	(rev (not org-log-states-order-reversed))
	(par-texts (org-element-map state-log 'paragraph
		     (lambda (p)
		       (buffer-substring
			(org-element-property :contents-begin p)
			(org-element-property :contents-end p)))))
	(note-match "Note taken on ")
	(state-match "State \"")	; good enough?
	(link-match "\\[\\[\\(gnus:\\|mailto:\\|bbdb:\\)")
	(count 0)
	candidates)
    (when rev
      (setq par-texts (nreverse par-texts)))
    (when (eq scope 'first)
      (setq scope 1))
    (catch 'bail
      (dolist (p par-texts)
	(when (or (and (not (eq type 'state))
		       (string-match-p note-match p))
		  (and (not (eq type 'note))
		       (string-match-p state-match p)))
	  (incf count)
	  (when (and (integerp scope)
		     (>= count scope)))
	  (when (string-match-p link-match p)
	    (push p candidates)
	    (when (eq scope 'first-link)
	      (throw 'bail t))))))
    (when candidates
      (gnorb-org-find-links candidates))))

(defun gnorb-org-find-links (strings &optional func)
  "Do the actual check to see if there are viable links in the
places we've decided to look."
  (when strings
    (when (not (listp strings))
      (setq strings (list strings)))
    (with-temp-buffer
      (dolist (s strings)
	(insert s)
	(insert "\n"))
      (goto-char (point-min))
      (if func
	  (funcall func (point-max))
	(gnorb-scan-links (point-max) 'gnus 'mail 'bbdb)))))

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
      (error (when (window-configuration-p gnorb-org-window-conf)
	       (set-window-configuration gnorb-org-window-conf))
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

;;;###autoload
(defun gnorb-org-handle-mail (&optional arg text file)
  "Handle current headline as a mail TODO."
  (interactive "P")
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
  (let* ((region
	  (when (use-region-p)
	    (cons (region-beginning) (region-end))))
	 ;; handle malformed values of `gnorb-org-mail-scan-strategies'
	 (strategy (cond
		    ((and region
			  arg)
		     'reverse-region)
		    (region
		     nil)
		    ((null arg)
		     (car gnorb-org-mail-scan-strategies))
		    ((equal '(4) arg)
		     (nth 1 gnorb-org-mail-scan-strategies))
		    ((equal '(16) arg)
		     (nth 2 gnorb-org-mail-scan-strategies)))))
    (deactivate-mark)
    (save-excursion
      (unless (org-back-to-heading t)
	(error "Not in an org item"))
      (cl-flet ((mp (p) (org-entry-get (point) p t)))
	(let* ((links (gnorb-org-extract-mail-stuff strategy region))
	       (attachments (gnorb-org-attachment-list))
	       (from (mp "MAIL_FROM"))
	       (cc (mp "MAIL_CC"))
	       (bcc (mp "MAIL_BCC"))
	       (org-id (org-id-get-create))
	       (recs (plist-get links :bbdb))
	       (message-mode-hook (copy-sequence message-mode-hook))
	       mails)
	  (when file
	    (cons g file attachments))
	  (when recs
	    (setq recs
		  (delete nil
			  (mapcar
			   (lambda (r)
			     (car (bbdb-message-search
				   (org-link-unescape r)
				   nil)))
			   recs))))
	  (when recs
	    (dolist (r recs)
	      (push (bbdb-mail-address r) mails)))
	  (when (and gnorb-bbdb-posting-styles
		     recs)
	    (add-hook 'message-mode-hook
		      (lambda ()
			(gnorb-bbdb-configure-posting-styles (cdr recs))
			(gnorb-bbdb-configure-posting-styles (list (car recs))))))
	  (gnorb-org-setup-message
	   (plist-get links :gnus)
	   (append mails (plist-get links :mail))
	   from cc bcc
	   attachments text org-id))))))

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
    (org-map-entries
     (lambda ()
       (setq props
	     (org-entry-get-multivalued-property
	      (point) gnorb-org-msg-id-key))
       (dolist (p props)
	 (gnorb-org-add-id-hash-entry p)))
     gnorb-org-find-candidates-match
     'agenda 'archive 'comment)))

(defun gnorb-org-find-visit-candidates (ids)
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
    (setq gnorb-org-window-conf (current-window-configuration))
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

(add-hook 'org-agenda-finalize-hook 'gnorb-org-popup-bbdb)

;;; Groups from the gnorb gnus server backend

;;;###autoload
(defun gnorb-org-view ()
  "Search the subtree at point for links to gnus messages, and
then show them in an ephemeral group, in gnus.

This won't work unless you've added a \"nngnorb\" server to
your gnus select methods."
  ;; this should also work on the active region, if there is one.
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
      (goto-char pos)
      (org-reveal)))
  (let (id)
    (save-excursion
      (org-back-to-heading)
      (setq id (concat "id+" (org-id-get-create t))))
    (gnorb-gnus-search-messages
     id
     `(when (window-configuration-p gnorb-org-window-conf)
	(set-window-configuration gnorb-org-window-conf)))))

(provide 'gnorb-org)
;;; gnorb-org.el ends here
