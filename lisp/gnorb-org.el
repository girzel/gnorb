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
  (when (eq major-mode 'gnus-summary-exit)
    (gnus-summary-exit nil t))
  (when (window-configuration-p gnorb-org-window-conf)
    (set-window-configuration gnorb-org-window-conf))
  (cond ((eq major-mode 'org-agenda-mode)
	 (if (null gnorb-org-mail-todos)
	     (call-interactively 'org-agenda-todo)
	   (let* ((marker (or (org-get-at-bol 'org-marker)
			      (org-agenda-error)))
		  (buffer (marker-buffer marker)))
	     (when (save-excursion
		     (with-current-buffer buffer
		       (goto-char (marker-position marker))
		       (member (org-entry-get (point) "TODO")
			       gnorb-org-mail-todos)))
	       (call-interactively 'org-agenda-todo)))))
	((eq major-mode 'org-mode)
	 (when (or (null gnorb-org-mail-todos)
		   (member (org-entry-get (point) "TODO")
			   gnorb-org-mail-todos))
	   (call-interactively 'org-todo)))
	(t nil)))

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

(defun gnorb-org-setup-message (message mails)
  (if (not message)
      (compose-mail (mapconcat 'identity mails ", ")
		    nil nil nil nil nil nil
		    '(gnorb-org-restore-after-send))
    (org-gnus-open (org-link-unescape (car message)))
    (call-interactively
     'gnus-summary-wide-reply-with-original)
    (when mails
      (message-goto-to)
      (insert ", ")
      (insert (mapconcat 'identity mails ", ")))
    (add-to-list 'message-exit-actions
		 'gnorb-org-restore-after-send t)
    (message-goto-body)))

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
  "Handle mail-related links for current headline."
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
	(attachments (gnorb-org-attachment-list)))
    (gnorb-org-setup-message (first mail-stuff) (second mail-stuff))
    (message-goto-body)
    (map-y-or-n-p
     (lambda (a)
       (format "Attach %s to outgoing message? "
	       (file-name-nondirectory a)))
     (lambda (a)
       (mml-attach-file
	a (mm-default-file-encoding a)
	nil "attachment"))
     attachments
     '("file" "files" "attach"))
    (if (second mail-stuff)
	(message-goto-body)
      (message-goto-to))))

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
	 (f-or-b (org-completing-read "Export as file or text? "
				      '("file" "text") nil t))
	 (org-export-show-temporary-export-buffer nil)
	 (opts (if (equal f-or-b "text")
		   gnorb-org-email-subtree-text-options
		 gnorb-org-email-subtree-file-options))
	 (result
	  (if (equal f-or-b "text")
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
	 (attachments (gnorb-org-attachment-list)))
    (setq gnorb-org-window-conf (current-window-configuration))
    (gnorb-org-setup-message (first mail-stuff) (second mail-stuff))
    (message-goto-body)
    (insert "\n")
    (if (equal f-or-b "text")
	(insert-buffer result)
      (mml-attach-file
       result
       (mm-default-file-encoding result)
       nil "attachment"))
    (map-y-or-n-p
     (lambda (a) (format "Attach %s to outgoing message? "
			 (file-name-nondirectory a)))
     (lambda (a)
       (mml-attach-file
	a (mm-default-file-encoding a)
	nil "attachment"))
     attachments
     '("file" "files" "attach"))
    (if (second mail-stuff)
	(message-goto-body)
      (message-goto-to))))

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

(provide 'gnorb-org)
;;; gnorb-org.el ends here
