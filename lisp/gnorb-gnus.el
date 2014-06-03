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

(defcustom gnorb-gnus-trigger-refile-targets
  '((org-agenda-files :maxlevel . 4))
  "A value to use as an equivalent of `org-refile-targets' (which
  see) when offering trigger targets for
  `gnorb-gnus-incoming-do-todo'."
  :group 'gnorb-gnus
  :type 'list)

;;; What follows is a very careful copy-pasta of bits and pieces from
;;; mm-decode.el and gnus-art.el. Voodoo was involved.

(defvar gnorb-gnus-capture-attachments nil
  "Holding place for attachment names during the capture
  process.")

(defun gnorb-gnus-article-org-attach (n)
  "Save MIME part N, which is the numerical prefix, of the
  article under point as an attachment to the specified org
  heading."
  (interactive "P")
  (gnus-article-part-wrapper n 'gnorb-gnus-attach-part))

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
	 ;; we should probably do the automatic location routine here,
	 ;; as well.
	 (org-refile-targets gnorb-gnus-trigger-refile-targets)
	 (org-heading (or org-heading
			  (org-refile-get-location "Attach part to" nil t))))
    (require 'org-attach)
    (save-window-excursion
      (find-file (nth 1 org-heading))
      (goto-char (nth 3 org-heading))
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

(defun gnorb-gnus-collect-all-attachments (&optional capture-p)
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
			    (lambda (h) (equal "attachment" (car (nth 5 h))))
			    gnus-article-mime-handle-alist) ))
      (when mime-handles
	(dolist (h mime-handles)
	  (let ((filename
		 (gnorb-gnus-save-part (cdr h))))
	    (when capture-p
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
     '("file" "files" "attach"))))

(add-hook 'org-capture-mode-hook 'gnorb-gnus-capture-attach)

;;; Try to save the capture heading id into the message as a custom
;;; header.

;; Actually, let's leave this as a WIP for now, and have a think about
;; it.

;; Possible workflow: multiple Org headers can be saved into a single
;; received Gnus message (but editing received messages is a pain in
;; the butt, and often doesn't work as expecte -- IMAP just makes new
;; messages). That can be done either by capturing from the message
;; (ie creating a new Org heading), or by using the (as-yet unwritten)
;; `gnorb-gnus-add-org-heading' (ie adding the ID of an existing Org
;; heading). If that message is replied to from within Gnus (you
;; didn't use `gnorb-org-handle-mail'), then all Org ID headers are
;; carried over into the reply, and then when the message is sent, all
;; the relevant IDs are prompted for TODO state-change. If you use
;; `gnorb-org-handle-mail' to reply to a message, then only the
;; heading you "depart" from gets prompted -- any other headings are
;; left alone.

;; Except maybe that doesn't make sense. Maybe all linked headings
;; should be visited and prompted. Hmm...

;; Also, when a message is sent, we should automatically push a link
;; to the sent message onto the link stack. That way, when we're
;; returned to the TODO and prompted for state change, a link to our
;; message can be inserted into the state-change log. Of course, that
;; only works if you're using archiving, and there's a Fcc header
;; present.

;; The model we're looking for is a single heading representing an
;; email conversation, bouncing back and forth between REPLY and WAIT
;; (for instance) states, with each state-change logged, and a link to
;; the relevant message inserted into each log line. This might not
;; even require editing received messages at all.

(defun gnorb-gnus-capture-abort-cleanup ()
  (when (and org-note-abort
	     (org-capture-get :gnus-attachments))
    (condition-case error
	(progn (org-attach-delete-all)
	       (setq abort-note 'clean)
	       ;; remove any gnorb-mail-header values here
	       )
      ((error
	(setq abort-note 'dirty))))))

(add-hook 'org-capture-prepare-finalize-hook
	  'gnorb-gnus-capture-abort-cleanup)

;;; Storing, removing, and acting on Org headers in messages.

(defvar gnorb-gnus-sending-message-info nil
  "Place to store the To, Subject, Date, and Message-ID headers
  of the currently-sending or last-sent message.")

(defun gnorb-gnus-check-outgoing-headers ()
  "Save the value of the `gnorb-mail-header' for the current
message; multiple header values returned as a string. Also save
information about the outgoing message into
`gnorb-gnus-sending-message-info'."
  (save-restriction
    (message-narrow-to-headers)
    (let* ((org-ids (mail-fetch-field gnorb-mail-header nil nil t))
	   (msg-id (mail-fetch-field "Message-ID"))
	   (to (if (message-news-p)
		   (mail-fetch-field "Newsgroups")
		 (mail-fetch-field "To")))
	   (toname (nth 1 (mail-extract-address-components to)))
	   (toaddress (nth 2 (mail-extract-address-components to)))
	   (subject (mail-fetch-field "Subject"))
	   (date (mail-fetch-field "Date"))
	   ;; if we can get a link, that's awesome
	   (link (or (and (mail-fetch-field "Gcc")
			  (call-interactively 'org-store-link))
		     nil)))
      ;; if we can't, then save some information so we can fake it
      (setq gnorb-gnus-sending-message-info
	    `(:subject ,subject :msg-id ,msg-id
		       :to ,to :toname ,toname :toaddress ,toaddress
		       :link ,link :date ,date))
      (if org-ids
	  (progn
	    (require 'gnorb-org)
	    (setq gnorb-message-org-ids org-ids)
	    ;; `gnorb-org-setup-message' may have put this here, but
	    ;; if we're working from a draft, or triggering this from
	    ;; a reply, it might not be there yet
	    (add-to-list 'message-exit-actions
			 'gnorb-org-restore-after-send))
	(setq gnorb-message-org-ids nil)))))

(add-hook 'message-header-hook 'gnorb-gnus-check-outgoing-headers)

(defun gnorb-gnus-outgoing-do-todo (arg)
  "Call this function to use the message currently being composed
as an email todo action. If it's a new message, or a reply to a
message that isn't referenced by any TODOs, a new TODO will be
created. If it references an existing TODO, you'll be prompted to
trigger a state-change or a note on that TODO.

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
  (if (not (eq major-mode 'message-mode))
      (gnorb-gnus-outgoing-make-todo-1)
    (let ((ids (mail-fetch-field gnorb-mail-header nil nil t)))
      (add-to-list
       'message-exit-actions
       (if ids
	   'gnorb-org-restore-after-send
	 'gnorb-gnus-outgoing-make-todo-1)
       t)
      (message
       (if ids
	   "Message will trigger TODO state-changes after sending"
	 "A TODO will be made from this message after it's sent")))))

(defun gnorb-gnus-outgoing-make-todo-1 ()
  (unless gnorb-gnus-new-todo-capture-key
    (error "No capture template key set, customize gnorb-gnus-new-todo-capture-key"))
  (let* ((subject (plist-get gnorb-gnus-sending-message-info :subject))
	 (to (plist-get gnorb-gnus-sending-message-info :to))
	 (toname (plist-get gnorb-gnus-sending-message-info :toaddress))
	 (toaddress (plist-get gnorb-gnus-sending-message-info :toaddress))
	 (date (plist-get gnorb-gnus-sending-message-info :date))
	 (msg-id (plist-get gnorb-gnus-sending-message-info :msg-id))
	 (link (plist-get gnorb-gnus-sending-message-info :link))
	 ;; If we actually have a link, then make use of it.
	 ;; Otherwise, fake it.
	 (org-capture-link-is-already-stored
	  (or link t)))
    (setq org-store-link-plist
	  `(:subject ,subject :to ,to :toname ,toname
		     :toaddress ,toaddress :date ,date))
    (org-capture nil gnorb-gnus-new-todo-capture-key)
    (when msg-id
      (org-entry-put (point) gnorb-org-msg-id-key msg-id))))

;;; If an incoming message should trigger state-change for a Org todo,
;;; call this function on it.

(defun gnorb-gnus-incoming-do-todo (arg &optional id)
  "Call this function from a received gnus message to store a
link to the message, prompt for a related Org heading, visit the
heading, and either add a note or trigger a TODO state change.
Set `gnorb-trigger-todo-default' to 'note or 'todo (you can
get the non-default behavior by calling this function with a
prefix argument), or to 'prompt to always be prompted.

In some cases, Gnorb can guess for you which Org heading you
probably want to trigger, which can save some time. It does this
by looking in the References and In-Reply-To headers, and seeing
if any of the IDs there match the value of the
`gnorb-org-msg-id-key' property for any headings."
  (interactive "P")
  (if (not (memq major-mode '(gnus-summary-mode gnus-article-mode)))
      (error "Only works in gnus summary or article mode")
    ;; We should only store a link if it's not already at the head of
    ;; `org-stored-links'. There's some duplicate storage, at
    ;; present. Take a look at calling it non-interactively.
    (call-interactively 'org-store-link)
    (let* ((org-refile-targets gnorb-gnus-trigger-refile-targets)
	   (ref-msg-ids
	    (with-current-buffer gnus-original-article-buffer
	      (nnheader-narrow-to-headers)
	      (let ((all-refs
		     (concat (message-fetch-field "in-reply-to")
			     (message-fetch-field "references"))))
		(when all-refs
		  (gnus-extract-message-id-from-in-reply-to all-refs)))))
	   (offer-heading
	    (when (and (not id) ref-msg-ids)
	      ;; for now we're basically ignoring the fact that
	      ;; multiple candidates could exist; just do the first
	      ;; one.
	      (car (gnorb-org-find-visit-candidates
		    (list ref-msg-ids)))))
	   targ)
      ;; offer to attach attachments!
      (if id
	  (gnorb-trigger-todo-action arg id)
	(if (and offer-heading
		 (y-or-n-p (format "Trigger action on %s"
			    (org-format-outline-path (cadr offer-heading)))))
	    (gnorb-trigger-todo-action arg (car offer-heading))
	  (setq targ (org-refile-get-location
		      "Trigger heading" nil t))
	  (find-file (nth 1 targ))
	  (goto-char (nth 3 targ))
	  (gnorb-trigger-todo-action arg))
	(message
	 "Insert a link to the message with org-insert-link (%s)"
	 (mapconcat 'key-description
		    (where-is-internal 'org-insert-link) ", "))))))

(provide 'gnorb-gnus)
;;; gnorb-gnus.el ends here
