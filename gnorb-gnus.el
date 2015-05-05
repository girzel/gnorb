;;; gnorb-gnus.el --- The gnus-centric fuctions of gnorb

;; Copyright (C) 2014  Free Software Foundation, Inc.

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

(require 'gnus)
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

(defcustom gnorb-gnus-summary-tracked-mark "&"
  "Default mark to insert in the summary format line of articles
  that are already tracked by TODO headings."
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
	 (headers (gnus-data-header
		   (gnus-data-find
		    (gnus-summary-article-number))))
	 (tracked-headings (gnorb-find-tracked-headings headers))
	 (target-heading
	  (gnorb-choose-trigger-heading tracked-headings)))
    (require 'org-attach)
    (save-window-excursion
      (org-id-goto target-heading)
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

(defvar org-note-abort)

(defun gnorb-gnus-capture-abort-cleanup ()
  (with-no-warnings ; For `org-note-abort'
    (when (and org-note-abort
	       (or gnorb-gnus-capture-always-attach
		   (org-capture-get :gnus-attachments)))
     (condition-case error
	 (progn (org-attach-delete-all)
		(setq abort-note 'clean)
		;; remove any gnorb-mail-header values here
		)
       (error
	(setq abort-note 'dirty))))))

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
	    (add-to-list 'message-send-actions
			 'gnorb-org-restore-after-send t))
	(setq gnorb-message-org-ids nil)))))

(add-hook 'message-sent-hook 'gnorb-gnus-check-outgoing-headers t)

;;;###autoload
(defun gnorb-gnus-outgoing-do-todo (&optional arg)
  "Use this command to use the message currently being composed
as an email todo action.

If it's a new message, or a reply to a message that isn't
referenced by any TODOs, a new TODO will be created.

If it references an existing TODO, you'll be prompted to trigger
a state-change or a note on that TODO after the message is sent.

You can call it with a prefix arg to force choosing an Org
subtree to associate with.

If you've already called this command, but realize you made a
mistake, you can call this command with a double prefix to reset
the association.

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
	header-ids ref-ids rel-headings
	gnorb-window-conf in-reply-to)
    (when (equal arg '(4))
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
	(if (equal arg '(16))
	    (user-error "A double prefix is only useful with an
	    unsent message.")
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
	      (gnorb-gnus-outgoing-make-todo-1))))
      ;; We are still in the message composition buffer, so let's see
      ;; what we've got.

      (if (equal arg '(16))
	  ;; Double prefix arg means delete the association we already
	  ;; made.
	  (save-excursion
	    (save-restriction
	      (widen)
	      (setq message-send-actions
		    (remove 'gnorb-gnus-outgoing-make-todo-1
			    message-send-actions))
	      (message-narrow-to-headers-or-head)
	      (message-remove-header
	       gnorb-mail-header)
	      (message "Message associations have been reset")))
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
	(unless header-ids
	  (add-to-list 'message-send-actions
	   'gnorb-gnus-outgoing-make-todo-1 t))
	(message
	 (if header-ids
	     "Message will trigger TODO state-changes after sending"
	   "A TODO will be made from this message after it's sent"))))))

(defvar org-capture-link-is-already-stored)

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
(defun gnorb-gnus-incoming-do-todo (arg &optional id)
  "Call this function from a received gnus message to store a
link to the message, prompt for a related Org heading, visit the
heading, and trigger an action on it \(see
`gnorb-org-trigger-actions'\).

If you've set up message tracking \(with
`gnorb-tracking-initialize'\), Gnorb can guess which Org heading
you probably want to trigger, which can save some time. It does
this by looking in the References header, and seeing if any of
the messages referenced there are already being tracked by any
headings.

If you mark several messages before calling this function, or
call it with a numerical prefix arg, those messages will be
\"bulk associated\" with the chosen Org heading: associations
will be made, but you won't be prompted to trigger an action, and
you'll stay in the Gnus summary buffer."
  (interactive "P")
  (when (not (memq major-mode '(gnus-summary-mode gnus-article-mode)))
    (user-error "Only works in gnus summary or article mode"))
  ;; We should only store a link if it's not already at the head of
  ;; `org-stored-links'. There's some duplicate storage, at
  ;; present. Take a look at calling it non-interactively.
  (setq gnorb-window-conf (current-window-configuration))
  (move-marker gnorb-return-marker (point))
  (setq gnorb-gnus-message-info nil)
  (let* ((articles (gnus-summary-work-articles arg))
	 (art-no (gnus-summary-article-number))
	 (headers (gnus-data-header
		   (gnus-data-find art-no)))
	 (msg-id (mail-header-id headers))
	 (from (mail-header-from headers))
	 (subject (mail-header-subject headers))
	 (date (mail-header-date headers))
	 (to (cdr (assoc 'To (mail-header-extra headers))))
	 (group (gnorb-get-real-group-name
		 gnus-newsgroup-name
		 art-no))
	 (link (call-interactively 'org-store-link))
	 (org-refile-targets gnorb-gnus-trigger-refile-targets)
	 (ref-msg-ids (concat (mail-header-references headers) " "
			      msg-id))
	 (related-headings
	  (when (and (null id) ref-msg-ids)
	    ;; Specifically ask for zombies, so the user has chance to
	    ;; flush them out.
	    (gnorb-find-tracked-headings headers t)))
	 targ)
    (setq gnorb-gnus-message-info
	  `(:subject ,subject :msg-id ,msg-id
		     :to ,to :from ,from
		     :link ,link :date ,date :refs ,ref-msg-ids
		     :group ,group))
    (gnorb-gnus-collect-all-attachments nil t)
    (condition-case err
     (if id
	 (progn
	   (delete-other-windows)
	   (gnorb-trigger-todo-action nil id))
       ;; Flush out zombies (dead associations).
       (setq related-headings
	     (cl-remove-if
	      (lambda (h)
		(when (null (org-id-find-id-file h))
		  (when (y-or-n-p
			 (format
			  "ID %s no longer exists, disassociate message?"
			  h))
		    (gnorb-delete-association msg-id h))))
	      related-headings))
       ;; See if one of the related headings is chosen.
       (unless (catch 'target
		 (dolist (h related-headings nil)
		   (when (yes-or-no-p
			  (format "Trigger action on %s"
				  (gnorb-pretty-outline h)))
		     (throw 'target (setq targ h)))))
	 ;; If not, use the refile interface to choose one.
	 (setq targ (org-refile-get-location
		     "Trigger heading" nil t))
	 (setq targ
	       (save-window-excursion
		 (find-file (nth 1 targ))
		 (goto-char (nth 3 targ))
		 (org-id-get-create))))
       ;; Either bulk associate multiple messages...
       (if (> (length articles) 1)
	   (progn
	     (dolist (a articles)
	       (gnorb-registry-make-entry
		(mail-header-id
		 (gnus-data-header
		  (gnus-data-find a)))
		from subject targ group)
	       (gnus-summary-remove-process-mark a))
	     (message "Associated %d messages with %s"
		      (length articles) (gnorb-pretty-outline targ)))
	 ;; ...or just trigger the one.
	 (delete-other-windows)
	 (gnorb-trigger-todo-action nil targ)))
     (error
      ;; If these are left populated after an error, it plays hell
      ;; with future trigger processes.
      (setq gnorb-gnus-message-info nil)
      (setq gnorb-gnus-capture-attachments nil)
      (signal (car err) (cdr err))))))

;;;###autoload
(defun gnorb-gnus-quick-reply ()
  "Compose a reply to the message under point, and associate both
the original message and the reply with the selected heading.
Take no other action.

Use this when you want to compose a reply to a message on the
spot, and track both messages, without having to go through the
hassle of triggering an action on a heading, and then starting a
reply."
  (interactive)
  (when (not (memq major-mode '(gnus-summary-mode gnus-article-mode)))
    (user-error "Only works in gnus summary or article mode"))
  (let* ((art-no (gnus-summary-article-number))
	 (headers (gnus-data-header
		   (gnus-data-find art-no)))
	 (msg-id (mail-header-id headers))
	 (from (mail-header-from headers))
	 (subject (mail-header-subject headers))
	 (group (gnorb-get-real-group-name
		 gnus-newsgroup-name
		 art-no))
	 (ref-msg-ids (concat (mail-header-references headers) " "
			      msg-id))
	 (related-headings
	  (when ref-msg-ids
	    (gnorb-find-tracked-headings headers t)))
	 (targ (car-safe related-headings)))
    (if targ
	(let ((ret (make-marker)))
	  (setq gnorb-window-conf (current-window-configuration))
	  (move-marker gnorb-return-marker (point))
	  ;; Assume the first heading is the one we want.
	  (gnorb-registry-make-entry
	   msg-id from subject targ group)
	  (gnus-summary-wide-reply-with-original 1)
	  (move-marker ret (point))
	  (save-restriction
	    (widen)
	    (message-narrow-to-headers-or-head)
	    (goto-char (point-min))
	    (open-line 1)
	    (message-insert-header
	     (intern gnorb-mail-header) targ))
	  (goto-char ret)
	  (message
	   (format "Original message and reply will be associated with %s"
		   (gnorb-pretty-outline targ))))
      (message "No associated headings found"))))

;;;###autoload
(defun gnorb-gnus-search-messages (str persist &optional head-text ret)
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
  (require 'nnir)
  (let ((nnir-address
	 (or (gnus-method-to-server '(nngnorb))
	     (user-error
	      "Please add a \"nngnorb\" backend to your gnus installation.")))
	name method spec)
    (when (version= "5.13" gnus-version-number)
      (with-no-warnings		  ; All these variables are available.
	(setq nnir-current-query nil
	      nnir-current-server nil
	      nnir-current-group-marked nil
	      nnir-artlist nil)))
    ;; In 24.4, the group name is mostly decorative, but in 24.3, the
    ;; actual query is held there.
    (setq name (if (version= "5.13" gnus-version-number)
		   (concat "nnir:" (prin1-to-string `((query ,str))))
		 (if persist
		     (read-string
		      (format "Name for group (default %s): " head-text)
		      nil head-text t)
		   (concat "gnorb-" str))))
    (setq method (if (version= "5.13" gnus-version-number)
		     (list 'nnir nnir-address)
		   (list 'nnir "Gnorb")))
    (setq spec
	  (list
	   (cons 'nnir-specs (list (cons 'nnir-query-spec `((query . ,str)))
				   (cons 'nnir-group-spec `((,nnir-address nil)))))
	   (cons 'nnir-artlist nil)))
    (if persist
	(progn
	  (switch-to-buffer gnus-group-buffer)
	  (gnus-group-make-group name method nil spec)
	  (gnus-group-select-group))
     (gnus-group-read-ephemeral-group name method nil ret nil nil spec))))

(defun gnorb-gnus-summary-mode-hook ()
  "Check if we've entered a Gnorb-generated group, and activate
  `gnorb-summary-minor-mode', if so."
  (let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
    (when (string-match-p "Gnorb" (cadr method))
      (gnorb-summary-minor-mode))))

(add-hook 'gnus-summary-mode-hook #'gnorb-gnus-summary-mode-hook)

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
  (when (and gnorb-gnus-hint-relevant-article
	     (not (memq (car (gnus-find-method-for-group
			      gnus-newsgroup-name))
			'(nnvirtual nnir))))
    (let* ((headers
	    (gnus-data-header
	     (gnus-data-find
	      (gnus-summary-article-number))))
	   (assoc-heading
	    (gnus-registry-get-id-key
	     (gnus-fetch-original-field "message-id") 'gnorb-ids))
	   (tracked-headings (gnorb-find-tracked-headings headers))
	   (key
	    (where-is-internal 'gnorb-gnus-incoming-do-todo
			       nil t)))
      (cond (assoc-heading
	     (message "Message is associated with %s"
		      (gnorb-pretty-outline (car assoc-heading) t)))
	    (tracked-headings
	     (message "Possible relevant todo %s, trigger with %s"
		      (gnorb-pretty-outline (car tracked-headings) t)
		      (if key
			  (key-description key)
			"M-x gnorb-gnus-incoming-do-todo")))
	    (t nil)))))

(add-hook 'gnus-article-prepare-hook 'gnorb-gnus-hint-relevant-message)

(defun gnorb-gnus-insert-format-letter-maybe (header)
  (if (not (memq (car (gnus-find-method-for-group
		       gnus-newsgroup-name))
		 '(nnvirtual nnir)))
      (cond ((gnus-registry-get-id-key
	      (mail-header-message-id header) 'gnorb-ids)
	     gnorb-gnus-summary-tracked-mark)
	    ((gnorb-find-tracked-headings header)
	     gnorb-gnus-summary-mark)
	    (t " "))
    " "))

(fset (intern (concat "gnus-user-format-function-"
		      gnorb-gnus-summary-mark-format-letter))
      (lambda (header)
	(gnorb-gnus-insert-format-letter-maybe header)))

;;;###autoload
(defun gnorb-gnus-view ()
  "Display the first relevant TODO heading for the message under point"
  (interactive)
  (let* ((headers (gnus-data-header
		   (gnus-data-find
		    (gnus-summary-article-number))))
	 (tracked-headings
	  (gnorb-find-tracked-headings headers)))
    (when tracked-headings
      (setq gnorb-window-conf (current-window-configuration))
      (move-marker gnorb-return-marker (point))
      (delete-other-windows)
      (org-id-goto (car tracked-headings)))))

(provide 'gnorb-gnus)
;;; gnorb-gnus.el ends here
