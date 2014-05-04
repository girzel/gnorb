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

(defun gnorb-org-contact-link (rec)
  "Prompt for a BBDB record and insert a link to that record at
point."
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
  (when (eq major-mode 'gnus-summary-mode)
    (gnus-summary-exit nil t))
  (when (window-configuration-p gnorb-org-window-conf)
    (set-window-configuration gnorb-org-window-conf))
  ;; This is some ugly stuff, but it (hopefully) results in smooth
  ;; usage.
  (let* ((agenda-p (eq major-mode 'org-agenda-mode))
	 ;; all this is still broken. Why?
	 (ret-dest-id (org-entry-get
		       (if agenda-p
			   (org-get-at-bol 'org-hd-marker)
			 (point-at-bol)) "ID"))
	 (ret-dest-todo (org-entry-get
			 (if agenda-p
			     (org-get-at-bol 'org-hd-marker)
			   (point-at-bol)) "TODO")))
    (if (not gnorb-message-org-ids)
	;; If there were no Org ID headers, just throw us back where
	;; we were and hope.
	(cond ((eq major-mode 'org-agenda-mode)
	       (call-interactively 'org-agenda-todo))
	      ((eq major-mode 'org-mode)
	       (call-interactively 'org-todo))
	      (t nil))
      ;; We've been returned to a heading, is it the one referenced by
      ;; the first of the Org ID headers? This should be the most
      ;; common case.
      (when (and (equal ret-dest-id (car gnorb-message-org-ids))
		 (or (null gnorb-org-mail-todos)
		     (member ret-dest-todo gnorb-org-mail-todos)))
	(if agenda-p
	    (call-interactively 'org-agenda-todo)
	  (call-interactively 'org-todo))
	(setq gnorb-message-org-ids
	      (cdr gnorb-message-org-ids)))
      ;; there's currently no way to attach multiple Org IDs to a
      ;; message, but there might be in the future
      (dolist (id gnorb-message-org-ids)
	(with-demoted-errors
	  (org-id-goto id)
	  (delete-other-windows)
	  (when (or (null gnorb-org-mail-todos)
		    (member (org-entry-get (point) "TODO")
			    gnorb-org-mail-todos))
	    (call-interactively 'org-todo))))))
  ;; this is a little unnecessary, but still...
  (setq gnorb-org-window-conf nil)
  (setq gnorb-message-org-ids nil))

(defun gnorb-org-extract-mail-stuff ()
  (let (message mails)
    (while (re-search-forward org-any-link-re (line-end-position) t)
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
		(push mail mails))))))))
    (list message mails)))

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
    (message-goto-to)))

(defun gnorb-org-attachment-list ()
  "Get a list of files (absolute filenames) attached to the
current heading."
  (when (featurep 'org-attach)
    (let* ((attach-dir (org-attach-dir t))
	   (files
	    (mapcar (lambda (f)
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
  (let ((mail-stuff (gnorb-org-extract-mail-stuff))
	(attachments (gnorb-org-attachment-list))
	(org-id (org-id-get-create)))
    (gnorb-org-setup-message
     (first mail-stuff) (second mail-stuff)
     attachments nil org-id)))

;;; Email subtree

(defcustom gnorb-org-email-subtree-parameters nil
  "A plist of export parameters corresponding to the EXT-PLIST
  argument to the export functions."
  :group 'gnorb-org)

(defcustom gnorb-org-email-subtree-text-options '(nil t nil t)
  "A list of ts and nils corresponding to Org's export options,
to be used when exporting to text. The options, in order, are
async, subtreep, visible-only, and body-only."
  :group 'gnorb-org)

(defcustom gnorb-org-email-subtree-file-options '(nil t nil nil)
  "A list of ts and nils corresponding to Org's export options,
to be used when exporting to a file. The options, in order, are
async, subtreep, visible-only, and body-only."
  :group 'gnorb-org)

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
		       ,gnorb-org-email-subtree-parameters))
	    (apply 'org-export-to-file
		   `(,backend-symbol
		     ,(org-export-output-file-name
		       (second (assoc backend-symbol gnorb-org-export-extensions))
		       t gnorb-tmp-dir)
		     ,@opts
		     ,gnorb-org-email-subtree-parameters))))
	 (mail-stuff (gnorb-org-extract-mail-stuff))
	 (attachments (gnorb-org-attachment-list))
	 (org-id (org-id-get-create))
	 text)
    (setq gnorb-org-window-conf (current-window-configuration))
    (if (bufferp result)
	(setq text result)
      (push result attachments))
    (gnorb-org-setup-message
     (first mail-stuff) (second mail-stuff)
     attachments text org-id)))

(defcustom gnorb-org-capture-collect-link-p t
  "Should the capture process store a link to the gnus message or
  BBDB record under point, even if it's not part of the
  template?"
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
