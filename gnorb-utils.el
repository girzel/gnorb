;;; gnorb-utils.el --- Common utilities for all gnorb stuff.

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

(require 'cl-lib)

(require 'mailcap)
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

(defvar gnorb-trigger-capture-location nil
  "Marker pointing at the location where we want to place capture
  templates, for the capture-to-child and capture-to-sibling
  trigger actions.")

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

;;;###autoload
(defun gnorb-restore-layout ()
  "Restore window layout and value of point after a Gnorb command.

Some Gnorb commands change the window layout (ie `gnorb-org-view'
or incoming email triggering). This command restores the layout
to what it was. Bind it to a global key, or to local keys in Org
and Gnus and BBDB maps."
  (interactive)
  (when (window-configuration-p gnorb-window-conf)
    (select-frame-set-input-focus
     (window-configuration-frame gnorb-window-conf))
    (set-window-configuration gnorb-window-conf)
    (when (buffer-live-p (marker-buffer gnorb-return-marker))
      (goto-char gnorb-return-marker))))

(defun gnorb-bracket-message-id (id)
  "Ensure message-id ID is bound by angle brackets."
  ;; Always use a message-id with angle brackets around it.
  ;; `gnus-summary-goto-article' can handle either, but
  ;; `gnus-request-head' will fail without brackets IF you're
  ;; requesting from an nntp group. Mysterious.
  (unless (string-match "\\`<" id)
    (setq id (concat "<" id)))
  (unless (string-match ">\\'" id)
    (setq id (concat id ">")))
  id)

(defun gnorb-unbracket-message-id (id)
  "Ensure message-id ID is NOT bound by angle brackets."
  ;; This shit is annoying, but Org wants an id with no brackets, and
  ;; Gnus is safest with an id that has brackets. So here we are.
  (replace-regexp-in-string "\\(\\`<\\|>\\'\\)" "" id))

(defun gnorb-reply-to-gnus-link (link)
  "Start a reply to the linked message."
  (let* ((link (org-link-unescape link))
	 (group (car (org-split-string link "#")))
	 (id (gnorb-bracket-message-id
	      (second (org-split-string link "#"))))
	 (backend
	  (car (gnus-find-method-for-group group))))
    (gnorb-follow-gnus-link group id)
    (call-interactively
     (if (eq backend 'nntp)
	 'gnus-summary-followup-with-original
       'gnus-summary-wide-reply-with-original))))

(defun gnorb-follow-gnus-link (group id)
  "Be a little clever about following gnus links.

The goal here is reuse frames and windows as much as possible, so
we're not opening multiple windows on the *Group* buffer, for
instance, and messing up people's layouts. There also seems to be
an issue when opening a link to a message whose *Summary* buffer
is already visible: somehow, after following the link, point ends
up on the message _after_ the one we want, and things go haywire.

So we try to be a little clever. The logical progression here is
this:

1. If the link's target group is already open in a *Summary*
buffer, just switch to that buffer (if it's visible in any frame
then raise it and switch focus, otherwise pull it into the
current window) and go to the message with
`gnus-summary-goto-article'.

2. If the Gnus *Group* buffer is visible in any window or frame,
raise that frame/window and give it focus before following the
link.

3. Otherwise just follow the link as usual, in the current
window."
  (let* ((sum-buffer (gnus-summary-buffer-name group))
	 (target-buffer
	  (cond
	   ((gnus-buffer-exists-p sum-buffer)
	    sum-buffer)
	   ((gnus-buffer-exists-p gnus-group-buffer)
	    gnus-group-buffer)
	   (t nil)))
	 (target-window (when target-buffer
			  (get-buffer-window target-buffer t))))
    (if target-window
	;; Our target buffer is displayed somewhere: just go there.
	(progn
	  (select-frame-set-input-focus
	   (window-frame target-window))
	  (switch-to-buffer target-buffer))
      ;; Our target buffer exists, but isn't displayed: pull it up.
      (if target-buffer
	  (switch-to-buffer target-buffer)))
    (message "Following link...")
    (if (gnus-buffer-exists-p sum-buffer)
	(gnus-summary-goto-article id nil t)
      (gnorb-open-gnus-link group id))))

(defun gnorb-open-gnus-link (group id)
  "Gnorb version of `org-gnus-follow-link'."
  ;; We've probably already bracketed the id, but just in case this is
  ;; called from elsewhere...
  (let* ((id (gnorb-bracket-message-id id))
	 (art-no (cdr (gnus-request-head id group)))
	 (arts (gnus-group-unread group))
	 success)
    (gnus-activate-group group)
    (setq success (gnus-group-read-group arts t group))
    (if success
	(gnus-summary-goto-article (or art-no id) nil t)
      (signal 'error "Group could not be opened."))))

(defun gnorb-trigger-todo-action (arg &optional id)
  "Do the actual restore action. Two main things here. First: if
we were in the agenda when this was called, then keep us in the
agenda. Then let the user choose an action from the value of
`gnorb-org-trigger-actions'."
  (let* ((agenda-p (eq major-mode 'org-agenda-mode))
	 (root-marker
	  (cond (agenda-p
		 (copy-marker
		  (org-get-at-bol 'org-hd-marker)))
		((derived-mode-p 'org-mode)
		 (save-excursion
		   (org-back-to-heading)
		   (point-marker)))
		(id
		 (save-excursion
		   (org-id-goto id)
		   (org-back-to-heading)
		   (point-marker)))))
	 (id (or id
		 (org-with-point-at root-marker
		   (org-id-get-create))))
	 (action (cdr (assoc
		       (org-completing-read
			(format
			 "Trigger action on %s: "
			 (gnorb-pretty-outline id))
			gnorb-org-trigger-actions nil t)
		       gnorb-org-trigger-actions))))
    (unless agenda-p
      (org-reveal))
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
      (if (fboundp action)
	  (org-with-point-at root-marker
	    (make-entry (org-id-get-create))
	    (funcall action gnorb-gnus-message-info))
	(cl-case action
	  (note
	   (org-with-point-at root-marker
	     (make-entry (org-id-get-create))
	     (call-interactively 'org-add-note)))
	  (todo
	   (if agenda-p
	       (progn
		 (org-with-point-at root-marker
		   (make-entry (org-id-get-create)))
		 (call-interactively 'org-agenda-todo))
	     (org-with-point-at root-marker
	       (make-entry (org-id-get-create))
	       (call-interactively 'org-todo))))
	  (no-associate
	   nil)
	  (associate
	   (org-with-point-at root-marker
	     (make-entry (org-id-get-create))))
	  ;; We're going to capture a new heading
	  ((cap-child cap-sib)
	   (org-with-point-at root-marker
		(setq gnorb-trigger-capture-location (point-marker)))
	   (let ((entry
		  ;; Pick a template.
		  (copy-sequence (org-capture-select-template))))
	     ;; Do surgery on that template so that it finds its
	     ;; location using our function.
	     (setf (nth 3 entry)
		   `(function
		     ,(if (eq action 'cap-child)
			  #'gnorb-trigger-capture-child
			#'gnorb-trigger-capture-sibling)))
	     ;; This will likely fail horribly for capture templates
	     ;; that aren't entries or list items.
	     (let ((org-capture-entry entry))
	       ;; When org-capture-entry is let-bound, the capture
	       ;; process will use that template instead of
	       ;; prompting the user. Also, `gnorb-registry-capture'
	       ;; will take care of making the registry entry for us.
	       (call-interactively 'org-capture)))))))
    ;; Lastly, query about attaching email attachments. No matter what
    ;; happens, clear `gnorb-gnus-capture-attachments'.
    (unwind-protect
	(org-with-point-at
	    (if (memq action '(cap-child cap-sib))
		(point)
	      root-marker)
	  (map-y-or-n-p
	   (lambda (a)
	     (format "Attach %s to heading? "
		     (file-name-nondirectory a)))
	   (lambda (a)
	     (with-demoted-errors
		 (org-attach-attach a nil 'mv)))
	   gnorb-gnus-capture-attachments
	   '("file" "files" "attach")))
      (setq gnorb-gnus-capture-attachments nil))))

(defun gnorb-trigger-capture-child ()
  ;; The capture process creates a child by default
  (org-goto-marker-or-bmk gnorb-trigger-capture-location)
  (org-back-to-heading))

(defun gnorb-trigger-capture-sibling ()
  ;; This only works if we're not trying to create a sibling for a
  ;; top-level heading, there appears to be no way to do that.  But in
  ;; that case this trigger action isn't really necessary, just
  ;; handle it with a regular capture.
  (org-goto-marker-or-bmk gnorb-trigger-capture-location)
  (org-up-heading-safe))

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
      (org-link-escape
       (concat server-group "#"
	       (gnorb-unbracket-message-id msg-id))))))

(defun gnorb-msg-id-to-group (msg-id)
  "Given a message id, try to find the group it's in.

So far we're checking the registry, then the groups in
`gnorb-gnus-sent-groups'. Use search engines? Other clever
methods?"
  (let (candidates server-group)
    (setq msg-id (gnorb-bracket-message-id msg-id))
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
      nil)))

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

;; Common functions for extracting references and relevant headings
;; from the message under point. For use in gnorb-gnus.el functions.

(defun gnorb-get-real-group-name (group art-no)
  "Find the original group name of a message in a virtual or nnir
group."
  (cl-case (car (gnus-find-method-for-group group))
    (nnvirtual
     (setq group (car (nnvirtual-map-article art-no))))
    (nnir
     (setq group (nnir-article-group art-no))))
  group)

(defun gnorb-find-tracked-headings (headers &optional include-zombies)
  "Check HEADERS for message references and return relevant heading IDs.

HEADERs is a message's data header, as produced by
\(gnus-interactive \"H\"\), or, equivalently:

\(gnus-data-header \(gnus-data-find \(gnus-summary-article-number\)\)\)"
  (let ((references (mail-header-references headers))
	(msg-id (mail-header-message-id headers)))
    (when gnorb-tracking-enabled
      (gnorb-find-visit-candidates
       (concat msg-id " " references) include-zombies))))

(defun gnorb-choose-trigger-heading (&optional id)
  "Given an Org heading ID, ask the user if they want to trigger it.

If not, prompt for another target heading. Either way, return the
target heading id."
  (let ((id (if (stringp id)
		id
	      (car-safe id)))
	refile-result)
    (if (and id
	     (y-or-n-p (message
			"Attach part to %s"
			(gnorb-pretty-outline id))))
	id
      (setq refile-result
	    (org-refile-get-location "Attach part to" nil t))
      (save-window-excursion
	(find-file (nth 1 refile-result))
	(goto-char (nth 3 refile-result))
	(org-id-get-create)))))

;; Loading the registry

(defvar gnorb-tracking-enabled nil
  "Internal flag indicating whether Gnorb is successfully plugged
  into the registry or not.")

;;;###autoload
(defun gnorb-tracking-initialize ()
  "Start using the Gnus registry to track correspondences between
Gnus messages and Org headings. This requires that the Gnus
registry be in use, and should be called after the call to
`gnus-registry-initialize'."
  (require 'gnorb-registry)
  (with-eval-after-load 'gnus-registry
    (add-to-list 'gnus-registry-extra-entries-precious 'gnorb-ids)
    (add-to-list 'gnus-registry-track-extra 'gnorb-ids))
  (add-hook
   'gnus-started-hook
   (lambda ()
     (unless (gnus-registry-install-p)
       (user-error "Gnorb tracking requires that the Gnus registry be installed."))
     (add-hook 'org-capture-mode-hook 'gnorb-registry-capture)
     (add-hook 'org-capture-prepare-finalize-hook 'gnorb-registry-capture-abort-cleanup)
     (setq gnorb-tracking-enabled t))))

(provide 'gnorb-utils)
;;; gnorb-utils.el ends here
