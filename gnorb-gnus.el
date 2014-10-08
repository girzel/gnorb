;;; gnorb-gnus.el --- The gnus-centric fuctions of gnorb

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

(require 'gnorb-utils)

(declare-function org-gnus-article-link "org-gnus"
		  (group newsgroups message-id x-no-archive))
(declare-function org-gnus-follow-link "org-gnus"
		  (group article))

(defgroup gnorb-gnus nil
  "The Gnus bits of Gnorb."
  :tag "Gnorb Gnus"
  :group 'gnorb)


(defcustom gnorb-gnus-mail-search-backends
  '((notmuch (lambda (terms)
	       (mapconcat
		(lambda (m)
		  (replace-regexp-in-string "\\." "\\\\." m))
		terms " OR "))
	     notmuch-search)
    (mairix (lambda (terms)
	      (mapconcat 'identity
			 terms ","))
	    mairix-search)
    (namazu (lambda (terms)
	      (mapconcat 'identity
			 terms " or "))
	    namazu-search))
  "Various backends for mail search.

An alist of backends, where each element consists of three parts:
the symbol name of the backend, a lambda form which receives a
list of email addresses and returns a properly-formatted search
string, and the symbol name of the function used to initiate the
search."
  :group 'gnorb-gnus
  :type 'list)

(defcustom gnorb-gnus-mail-search-backend nil
  "Mail search backend currently in use. One of the three symbols
notmuch, namazu, or mairix."
  :group 'gnorb-gnus
  :type 'symbol)

(defcustom gnorb-gnus-capture-always-attach nil
  "Always prompt about attaching attachments when capturing from
  a Gnus message, even if the template being used hasn't
  specified the :gnus-attachments key.

Basically behave as if all attachments have \":gnus-attachments t\"."
  :group 'gnorb-gnus
  :type 'boolean)

(defcustom gnorb-gnus-new-todo-capture-key nil
  "Key for the capture template to use when creating a new TODO
  from an outgoing message."
  :group 'gnorb-gnus
  :type 'string)

(defcustom gnorb-gnus-hint-relevant-article t
  "When opening a gnus message, should gnorb let you know if the
  message is relevant to an existing TODO?"
  :group 'gnorb-gnus
  :type 'boolean)

(defcustom gnorb-gnus-summary-mark-format-letter "g"
  "Format letter to be used as part of your
  `gnus-summary-line-format', to indicate in the *Summary* buffer
  which articles might be relevant to TODOs. Since this is a user
  format code, it should be prefixed with %u, eg %ug. It will
  result in the insertion of the value of
  `gnorb-gnus-summary-mark', for relevant messages, or
  else a space."
  :group 'gnorb-gnus
  :type 'string)

(defcustom gnorb-gnus-summary-mark "¡"
  "Default mark to insert in the summary format line of articles
  that are likely relevant to existing TODO headings."
  :group 'gnorb-gnus
  :type 'string)

(defcustom gnorb-gnus-trigger-refile-targets
  '((org-agenda-files :maxlevel . 4))
  "A value to use as an equivalent of `org-refile-targets' (which
  see) when offering trigger targets for
  `gnorb-gnus-incoming-do-todo'."
  :group 'gnorb-gnus
  :type 'list)

(defcustom gnorb-gnus-sent-groups nil
  "A list of strings indicating sent mail groups.

In some cases, Gnorb can't detect where your sent messages are
stored (ie if you're using IMAP sent mail folders instead of
local archiving. If you want Gnorb to be able to find sent
messages, this option can help it do that. It should be set to a
list of strings, which are assumed to be fully qualified
server+group combinations, ie \"nnimap+Server:[Gmail]/Sent
Mail\", or something similar. This only has to be done once for
each message."
  :group 'gnorb-gnus
  :type 'list)

(defvar gnorb-gnus-capture-attachments nil
  "Holding place for attachment names during the capture
  process.")

;;; What follows is a very careful copy-pasta of bits and pieces from
;;; mm-decode.el and gnus-art.el. Voodoo was involved.

;;;###autoload
(defun gnorb-gnus-article-org-attach (n)
  "Save MIME part N, which is the numerical prefix, of the
  article under point as an attachment to the specified org
  heading."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnorb-gnus-attach-part))

;;;###autoload
(defun gnorb-gnus-mime-org-attach ()
  "Save the MIME part under point as an attachment to the
  specified org heading."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (gnorb-gnus-attach-part data))))

(defun gnorb-gnus-attach-part (handle &optional org-heading)
  "Attach HANDLE to an existing org heading."
  (let* ((filename (gnorb-gnus-save-part handle))
	 (org-refile-targets gnorb-gnus-trigger-refile-targets)
	 (ref-msg-ids
	  (concat (gnus-fetch-original-field "references") " "
		  (gnus-fetch-original-field "in-reply-to")))
	 (rel-heading
	  (when gnorb-tracking-enabled
	    (car (gnorb-find-visit-candidates
		  ref-msg-ids))))
	 (org-heading
	  (if (and rel-heading
		   (y-or-n-p (message
			      "Attach part to %s"
			      (gnorb-pretty-outline rel-heading))))
	      rel-heading
	    (org-refile-get-location "Attach part to" nil t))))
    (require 'org-attach)
    (save-window-excursion
      (if (stringp org-heading)
	  (org-id-goto org-heading)
	(progn
	  (find-file (nth 1 org-heading))
	  (goto-char (nth 3 org-heading))))
      (org-attach-attach filename nil 'mv))))

(defun gnorb-gnus-save-part (handle)
  (let ((filename (or (mail-content-type-get
		       (mm-handle-disposition handle) 'filename)
		      (mail-content-type-get
		       (mm-handle-type handle) 'name))))
    (setq filename
	  (gnus-map-function mm-file-name-rewrite-functions
			     (file-name-nondirectory filename)))
    (setq filename (expand-file-name filename gnorb-tmp-dir))
    (mm-save-part-to-file handle filename)
    filename))

(defun gnorb-gnus-collect-all-attachments (&optional capture-p store)
  "Collect all the attachments from the message under point, and
save them into `gnorb-tmp-dir'."
  (save-window-excursion
    (when capture-p
      (set-buffer (org-capture-get :original-buffer)))
    (unless (memq major-mode '(gnus-summary-mode gnus-article-mode))
      (error "Only works in Gnus summary or article buffers"))
    (let ((article (gnus-summary-article-number)) 
	  mime-handles)
      (when (or (null gnus-current-article)
		(null gnus-article-current)
		(/= article (cdr gnus-article-current))
		(not (equal (car gnus-article-current) gnus-newsgroup-name)))
	(gnus-summary-display-article article))
      (gnus-eval-in-buffer-window gnus-article-buffer
	(setq mime-handles (cl-remove-if-not
			    (lambda (h)
			      (let ((disp (mm-handle-disposition (cdr h))))
				(and (member (car disp)
					     '("inline" "attachment"))
				     (mail-content-type-get disp 'filename))))
			    gnus-article-mime-handle-alist)))
      (when mime-handles
	(dolist (h mime-handles)
	  (let ((filename
		 (gnorb-gnus-save-part (cdr h))))
	    (when (or capture-p store)
	      (push filename gnorb-gnus-capture-attachments))))))))

;;; Make the above work in the capture process

(defun gnorb-gnus-capture-attach ()
  (when (and (or gnorb-gnus-capture-always-attach
		 (org-capture-get :gnus-attachments))
	     (with-current-buffer
		 (org-capture-get :original-buffer)
	       (memq major-mode '(gnus-summary-mode gnus-article-mode))))
    (require 'org-attach)
    (setq gnorb-gnus-capture-attachments nil)
    (gnorb-gnus-collect-all-attachments t)
    (map-y-or-n-p
     (lambda (a)
       (format "Attach %s to capture heading? "
	       (file-name-nondirectory a)))
     (lambda (a) (org-attach-attach a nil 'mv))
     gnorb-gnus-capture-attachments
     '("file" "files" "attach"))
    (setq gnorb-gnus-capture-attachments nil)))

(add-hook 'org-capture-mode-hook 'gnorb-gnus-capture-attach)

(defun gnorb-gnus-capture-abort-cleanup ()
  (when (and org-note-abort
	     (org-capture-get :gnus-attachments))
    (condition-case error
	(progn (org-attach-delete-all)
	       (setq abort-note 'clean)
	       ;; remove any gnorb-mail-header values here
	       )
      (error
       (setq abort-note 'dirty)))))

(add-hook 'org-capture-prepare-finalize-hook
	  'gnorb-gnus-capture-abort-cleanup)

;;; Storing, removing, and acting on Org headers in messages.

(defvar gnorb-gnus-message-info nil
  "Place to store the To, Subject, Date, and Message-ID headers
  of the currently-sending or last-sent message.")

(defun gnorb-gnus-check-outgoing-headers ()
  "Save the value of the `gnorb-mail-header' for the current
message; multiple header values returned as a string. Also save
information about the outgoing message into
`gnorb-gnus-message-info'."
  (save-restriction
    (message-narrow-to-headers)
    (setq gnorb-gnus-message-info nil)
    (let* ((org-ids (mail-fetch-field gnorb-mail-header nil nil t))
	   (msg-id (mail-fetch-field "Message-ID"))
	   (refs (mail-fetch-field "References"))
	   (in-reply-to (mail-fetch-field "In-Reply-To"))
	   (to (if (message-news-p)
		   (mail-fetch-field "Newsgroups")
		 (mail-fetch-field "To")))
	   (from (mail-fetch-field "From"))
	   (subject (mail-fetch-field "Subject"))
	   (date (mail-fetch-field "Date"))
	   ;; If we can get a link, that's awesome.
	   (gcc (mail-fetch-field "Gcc"))
	   (link (or (and gcc
			  (org-store-link nil))
		     nil))
	   (group (ignore-errors (car (split-string link "#")))))
      ;; If we can't make a real link, then save some information so
      ;; we can fake it.
      (when in-reply-to
	(setq refs (concat refs " " in-reply-to)))
      (when refs
	(setq refs (gnus-extract-references refs)))
      (setq gnorb-gnus-message-info
	    `(:subject ,subject :msg-id ,msg-id
		       :to ,to :from ,from
		       :link ,link :date ,date :refs ,refs
		       :group ,group))
      (if org-ids
	  (progn
	    (require 'gnorb-org)
	    (setq gnorb-message-org-ids org-ids)
	    ;; `gnorb-org-setup-message' may have put this here, but
	    ;; if we're working from a draft, or triggering this from
	    ;; a reply, it might not be there yet.
	    (add-to-list 'message-exit-actions
			 'gnorb-org-restore-after-send t))
	(setq gnorb-message-org-ids nil)))))

(add-hook 'message-header-hook 'gnorb-gnus-check-outgoing-headers)

;;;###autoload
(defun gnorb-gnus-outgoing-do-todo (&optional arg)
  "Call this function to use the message currently being composed
as an email todo action. If it's a new message, or a reply to a
message that isn't referenced by any TODOs, a new TODO will be
created. If it references an existing TODO, you'll be prompted to
trigger a state-change or a note on that TODO.

Otherwise, you can call it with a prefix arg to associate the
sending/sent message with an existing Org subtree, and trigger an
action on that subtree.

If a new todo is made, it needs a capture template: set
`gnorb-gnus-new-todo-capture-key' to the string key for the
appropriate capture template. If you're using a gnus-based
archive method (ie you have `gnus-message-archive-group' set to
something, and your outgoing messages have a \"Fcc\" header),
then a real link will be made to the outgoing message, and all
the gnus-type escapes will be available (see the Info
manual (org) Template expansion section). If you don't, then the
%:subject, %:to, %:toname, %:toaddress, and %:date escapes for
the outgoing message will still be available -- nothing else will
work."
  (interactive "P")
  (let ((org-refile-targets gnorb-gnus-trigger-refile-targets)
	(compose-marker (make-marker))
	header-ids ref-ids rel-headings gnorb-window-conf
	reply-id reply-group in-reply-to)
    (when arg
      (setq rel-headings
	    (org-refile-get-location "Trigger action on" nil t))
      (setq rel-headings
	    (list (list (save-window-excursion
			  (find-file (nth 1 rel-headings))
			  (goto-char (nth 3 rel-headings))
			  (org-id-get-create))))))
    (if (not (eq major-mode 'message-mode))
	;; The message is already sent, so we're relying on whatever was
	;; stored into `gnorb-gnus-message-info'.
	(if arg
	    (progn
	      (push (car rel-headings) gnorb-message-org-ids)
	      (gnorb-org-restore-after-send))
	  (setq ref-ids (plist-get gnorb-gnus-message-info :refs))
	  (if ref-ids
	      ;; the message might be relevant to some TODO
	      ;; heading(s). But if there had been org-id
	      ;; headers, they would already have been
	      ;; handled when the message was sent.
	      (progn
		(setq rel-headings (gnorb-find-visit-candidates ref-ids))
		(if (not rel-headings)
		    (gnorb-gnus-outgoing-make-todo-1)
		  (dolist (h rel-headings)
		    (push h gnorb-message-org-ids))
		  (gnorb-org-restore-after-send)))
	    ;; not relevant, just make a new TODO
	    (gnorb-gnus-outgoing-make-todo-1)))
      ;; We are still in the message composition buffer, so let's see
      ;; what we've got.

      ;; What we want is a link to the original message we're replying
      ;; to, if this is actually a reply.
      (when message-reply-headers
	(setq reply-id (aref message-reply-headers 4)))
      ;; Save-excursion won't work, because point will move if we
      ;; insert headings.
      (move-marker compose-marker (point))
      (save-restriction
	(widen)
	(message-narrow-to-headers-or-head)
	(setq header-ids (mail-fetch-field gnorb-mail-header nil nil t))
	;; With a prefix arg we do not check references, because the
	;; whole point is to add new references. We still want to know
	;; what org id headers are present, though, so we don't add
	;; duplicates.
	(setq ref-ids (unless arg (mail-fetch-field "References" t)))
	(setq in-reply-to (unless arg (mail-fetch-field "In-Reply-to" t)))
	(when in-reply-to
	  (setq ref-ids (concat ref-ids " " in-reply-to)))
	(setq reply-group (when (mail-fetch-field "X-Draft-From" t)
			    (car-safe (read (mail-fetch-field "X-Draft-From" t)))))
	;; when it's a reply, store a link to the reply just in case.
	;; This is pretty embarrassing -- we follow a link just to
	;; create a link. But I'm not going to recreate all of
	;; `org-store-link' by hand.
	(when (and reply-group reply-id)
	  (save-window-excursion
	    (org-gnus-follow-link reply-group reply-id)
	    (call-interactively 'org-store-link)))
	(when ref-ids
	  ;; if the References header points to any message ids that are
	  ;; tracked by TODO headings...
	  (setq rel-headings (gnorb-find-visit-candidates ref-ids)))
	(when rel-headings
	  (goto-char (point-min))
	  (dolist (h (delete-dups rel-headings))
	    ;; then get the org-ids of those headings, and insert
	    ;; them into this message as headers. If the id was
	    ;; already present in a header, don't add it again.
	    (unless (member h header-ids)
	      (goto-char (point-at-bol))
	      (open-line 1)
	      (message-insert-header
	       (intern gnorb-mail-header)
	       h)
	      ;; tell the rest of the function that this is a relevant
	      ;; message
	      (push h header-ids)))))
      (goto-char compose-marker)
      (add-to-list
       'message-exit-actions
       (if header-ids
	   'gnorb-org-restore-after-send
	 'gnorb-gnus-outgoing-make-todo-1)
       t)
      (message
       (if header-ids
	   "Message will trigger TODO state-changes after sending"
	 "A TODO will be made from this message after it's sent")))))

(defun gnorb-gnus-outgoing-make-todo-1 ()
  (unless gnorb-gnus-new-todo-capture-key
    (error "No capture template key set, customize gnorb-gnus-new-todo-capture-key"))
  (let* ((link (plist-get gnorb-gnus-message-info :link))
	 (group (plist-get gnorb-gnus-message-info :group))
	 (date (plist-get gnorb-gnus-message-info :date))
	 (date-ts (and date
		       (ignore-errors
			 (format-time-string
			  (org-time-stamp-format t)
			  (date-to-time date)))))
	 (date-ts-ia (and date
			  (ignore-errors
			    (format-time-string
			     (org-time-stamp-format t t)
			     (date-to-time date)))))
	 (msg-id (plist-get gnorb-gnus-message-info :msg-id))
	 (sender (plist-get gnorb-gnus-message-info :from))
	 (subject (plist-get gnorb-gnus-message-info :subject))
	 ;; Convince Org we already have a link stored, even if we
	 ;; don't.
	 (org-capture-link-is-already-stored t))
    (if link
	;; Even if you make a link to not-yet-sent messages, even if
	;; you've saved the draft and it has a Date header, that
	;; header isn't saved into the link plist. So fake that, too.
	(org-add-link-props
	 :date date
	 :date-timestamp date-ts
	 :date-timestamp-inactive date-ts-ia
	 :annotation link)
      (org-store-link-props
       :subject (plist-get gnorb-gnus-message-info :subject)
       :to (plist-get gnorb-gnus-message-info :to)
       :date date
       :date-timestamp date-ts
       :date-timestamp-inactive date-ts-ia
       :message-id msg-id
       :annotation link))
    (org-capture nil gnorb-gnus-new-todo-capture-key)
    (when msg-id
      (org-entry-put (point) gnorb-org-msg-id-key msg-id)
      (gnorb-registry-make-entry msg-id sender subject (org-id-get-create) group))))

;;; If an incoming message should trigger state-change for a Org todo,
;;; call this function on it.

;;;###autoload
(defun gnorb-gnus-incoming-do-todo (arg headers &optional id)
  "Call this function from a received gnus message to store a
link to the message, prompt for a related Org heading, visit the
heading, and either add a note or trigger a TODO state change.
Set `gnorb-trigger-todo-default' to 'note or 'todo (you can
get the non-default behavior by calling this function with a
prefix argument), or to 'prompt to always be prompted.

In some cases, Gnorb can guess for you which Org heading you
probably want to trigger, which can save some time. It does this
by looking in the References header, and seeing if any of the IDs
there match the value of the `gnorb-org-msg-id-key' property for
any headings. In order for this to work, you will have to have
loaded org-id, and have the variable `org-id-track-globally' set
to t (it is, by default)."
  (interactive (gnus-interactive "P\nH"))
  (when (not (memq major-mode '(gnus-summary-mode gnus-article-mode)))
    (user-error "Only works in gnus summary or article mode"))
  ;; We should only store a link if it's not already at the head of
  ;; `org-stored-links'. There's some duplicate storage, at
  ;; present. Take a look at calling it non-interactively.
  (setq gnorb-window-conf (current-window-configuration))
  (move-marker gnorb-return-marker (point))
  (setq gnorb-gnus-message-info nil)
  (let* ((msg-id (mail-header-id headers))
	 (from (mail-header-from headers))
	 (subject (mail-header-subject headers))
	 (date (mail-header-date headers))
	 (to (cdr (assoc 'To (mail-header-extra headers))))
	 (group gnus-newsgroup-name)
	 (link (call-interactively 'org-store-link))
	 (org-refile-targets gnorb-gnus-trigger-refile-targets)
	 (ref-msg-ids (mail-header-references headers))
	 (offer-heading
	  (when (and (not id) ref-msg-ids gnorb-tracking-enabled)
	    (if org-id-track-globally
		;; for now we're basically ignoring the fact that
		;; multiple candidates could exist; just do the first
		;; one.
		(car (gnorb-find-visit-candidates
		      ref-msg-ids))
	      (message "Gnorb can't check for relevant headings unless `org-id-track-globally' is t")
	      (sit-for 1))))
	 targ)
    (setq gnorb-gnus-message-info
	    `(:subject ,subject :msg-id ,msg-id
		       :to ,to :from ,from
		       :link ,link :date ,date :refs ,ref-msg-ids
		       :group ,group))
    (gnorb-gnus-collect-all-attachments nil t)
    ;; Delete other windows, users can restore with
    ;; `gnorb-restore-layout'.
    (delete-other-windows)
    (if id
	(gnorb-trigger-todo-action arg id)
      (if (and offer-heading
	       (y-or-n-p (format "Trigger action on %s"
				 (gnorb-pretty-outline offer-heading))))
	  (gnorb-trigger-todo-action arg offer-heading)
	(setq targ (org-refile-get-location
		    "Trigger heading" nil t))
	(find-file (nth 1 targ))
	(goto-char (nth 3 targ))
	(gnorb-trigger-todo-action arg)))))

;;;###autoload
(defun gnorb-gnus-search-messages (str &optional ret)
  "Initiate a search for gnus message links in an org subtree.
The arg STR can be one of two things: an Org heading id value
\(IDs should be prefixed with \"id+\"\), in which case links will
be collected from that heading, or a string corresponding to an
Org tags search, in which case links will be collected from all
matching headings.

In either case, once a collection of links have been made, they
will all be displayed in an ephemeral group on the \"nngnorb\"
server. There must be an active \"nngnorb\" server for this to
work."
  (interactive)
  (let ((nnir-address
	 (or (gnus-method-to-server '(nngnorb))
	     (user-error
	      "Please add a \"nngnorb\" backend to your gnus installation."))))
    (when (version= "5.13" gnus-version-number)
      (setq nnir-current-query nil
	    nnir-current-server nil
	    nnir-current-group-marked nil
	    nnir-artlist nil))
    (gnus-group-read-ephemeral-group
     ;; in 24.4, the group name is mostly decorative. in 24.3, the
     ;; query itself is read from there. It should look like (concat
     ;; "nnir:" (prin1-to-string '((query str))))
     (if (version= "5.13" gnus-version-number)
	 (concat "nnir:" (prin1-to-string `((query ,str))))
       (concat "gnorb-" str))
     (if (version= "5.13" gnus-version-number)
	 (list 'nnir nnir-address)
       (list 'nnir "nnir"))
     nil
     ret ;; it's possible you can't just put an arbitrary form in
	 ;; here, which sucks.
     nil nil
     ;; the following seems to simply be ignored under gnus 5.13
     (list (cons 'nnir-specs (list (cons 'nnir-query-spec `((query . ,str)))
				   (cons 'nnir-group-spec `((,nnir-address nil)))))
	   (cons 'nnir-artlist nil)))
    (gnorb-summary-minor-mode)))

;;; Automatic noticing of relevant messages

;; likely hooks for the summary buffer include:
;; `gnus-parse-headers-hook'

;; BBDB puts its notice stuff in the `gnus-article-prepare-hook',
;; which seems as good a spot as any.

(defun gnorb-gnus-hint-relevant-message ()
  "When opening an article buffer, check the message to see if it
is relevant to any existing TODO headings. If so, flash a message
to that effect. This function is added to the
`gnus-article-prepare-hook'. It will only do anything if the
option `gnorb-gnus-hint-relevant-article' is non-nil."
  (when (and gnorb-tracking-enabled
	     gnorb-gnus-hint-relevant-article
	     (not (memq (car (gnus-find-method-for-group
			      gnus-newsgroup-name))
			'(nnvirtual nnir))))
    (let* ((ref-ids (concat
		     (gnus-fetch-original-field "references") " "
		     (gnus-fetch-original-field "in-reply-to")))
	   (msg-id (gnus-fetch-original-field "message-id"))
	   (assoc-heading
	    (gnus-registry-get-id-key msg-id 'gnorb-ids))
	   (key
	    (where-is-internal 'gnorb-gnus-incoming-do-todo
			       nil t))
	   rel-headings)
      (cond (assoc-heading
	     (message "Message is associated with %s"
		      (gnorb-pretty-outline (car assoc-heading) t)))
	    (ref-ids
	     (when (setq rel-headings
			 (gnorb-find-visit-candidates ref-ids))
	       (message "Possible relevant todo %s, trigger with %s"
			(gnorb-pretty-outline (car rel-headings) t)
			(if key
			    (key-description key)
			  "M-x gnorb-gnus-incoming-do-todo"))))))))

(add-hook 'gnus-article-prepare-hook 'gnorb-gnus-hint-relevant-message)

(defun gnorb-gnus-insert-format-letter-maybe (header)
  (if (and gnorb-tracking-enabled
		 (not (memq (car (gnus-find-method-for-group
				  gnus-newsgroup-name))
			    '(nnvirtual nnir))))
	    (let ((ref-ids (mail-header-references header))
		  (msg-id (mail-header-message-id header)))
	      (if (or (gnus-registry-get-id-key msg-id 'gnorb-ids)
		      (and ref-ids
			   (gnorb-find-visit-candidates ref-ids)))
		  gnorb-gnus-summary-mark
		" "))
	  " "))

(fset (intern (concat "gnus-user-format-function-"
		      gnorb-gnus-summary-mark-format-letter))
      (lambda (header)
	(gnorb-gnus-insert-format-letter-maybe header)))

;;;###autoload
(defun gnorb-gnus-view ()
  "Display the first relevant TODO heading for the message under point"
  ;; this is pretty barebones, need to make sure we have a valid
  ;; article buffer to access, and think about what to do for
  ;; window-configuration!

  ;; boy is this broken now.
  (interactive)
  (let ((refs (gnus-fetch-original-field "references"))
	rel-headings)
    (when refs
      (setq rel-headings (gnorb-find-visit-candidates refs))
      (delete-other-windows)
      (org-id-goto (car rel-headings)))))

(provide 'gnorb-gnus)
;;; gnorb-gnus.el ends here
