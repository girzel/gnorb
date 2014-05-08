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
  "Mail search backend currently in use."
  :group 'gnorb-gnus
  :type 'symbol)

(defcustom gnorb-gnus-capture-always-attach nil
  "Always prompt about attaching attachments when capturing from
  a Gnus message, even if the template being used hasn't
  specified the :gnus-attachments key.

Basically behave as if all attachments have \":gnus-attachments t\".")

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
  (let ((filename (gnorb-gnus-save-part handle))
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
;; the butt, and often doesn't work). That can be done either by
;; capturing from the message (ie creating a new Org heading), or by
;; using the (as-yet unwritten) `gnorb-gnus-add-org-heading' (ie
;; adding the ID of an existing Org heading). If that message is
;; replied to from within Gnus (you didn't use
;; `gnorb-org-handle-mail'), then all Org ID headers are carried over
;; into the reply, and then when the message is sent, all the relevant
;; IDs are prompted for TODO state-change. If you use
;; `gnorb-org-handle-mail' to reply to a message, then only the
;; heading you "depart" from gets prompted -- any other headings are
;; left alone.

;; Except maybe that doesn't make sense. Maybe all linked headings
;; should be visited and prompted. Hmm...

;; Also, when a message is sent, we should automatically push a link
;; to the sent message onto the link stack. That way, when we're
;; returned to the TODO and prompted for state change, a link to our
;; message can be inserted into the state-change log.

;; The model we're looking for is a single heading representing an
;; email conversation, bouncing back and forth between REPLY and WAIT
;; (for instance) sates, with each state-change logged, and a link to
;; the relevant message inserted into each log line. This might not
;; even require editing received messages at all.

;; (defun gnorb-gnus-insert-org-header ()
;;   (let ((id (org-id-get-create)))
;;     (with-current-buffer
;; 	(org-capture-get :original-buffer)
;;       (when (memq major-mode '(gnus-summary-mode gnus-article-mode)))
;;       (gnus-with-article-buffer
;; 	))))

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

(defun gnorb-gnus-check-org-header ()
  "Return the value of the `gnorb-gnus-org-header' for the
current message; multiple header values returned as a string."
  (save-restriction
    (message-narrow-to-headers)
    (let ((org-ids (mail-fetch-field gnorb-mail-header nil nil t)))
      (if org-ids
	  (progn
	    (require 'gnorb-org)
	    (setq gnorb-message-org-ids org-ids)
	    ;; `gnorb-org-setup-message' may have put this here, but
	    ;; if we're working from a draft or whatever, it might not
	    ;; be there yet
	    (add-to-list 'message-exit-actions
			 'gnorb-org-restore-after-send t))
	(setq gnorb-message-org-ids nil)))))

(add-hook 'message-send-hook 'gnorb-gnus-check-org-header)

;;; If an incoming message should trigger state-change for a Org todo,
;;; call this function on it.

(defcustom gnorb-gnus-message-trigger-default 'note
  "What default action should be taken when triggering TODO
  state-change from a message? Valid values are the symbols note
  and todo. Whatever the default is, giving the command a prefix
  argument will do the opposite."
  :group 'gnorb-gnus
  :type '(choice (const note)
		 (const todo)))

(defun gnorb-gnus-message-trigger-todo (arg &optional heading)
  "Call this function from a received gnus message to store a
link to the message, prompt for a related Org heading, visit the
heading, and either add a note or trigger a TODO state change.
Set `gnorb-gnus-message-trigger-default' to either 'note or
'todo; you can get the non-default behavior by calling this
function with a prefix argument."
  ;; this whole function isn't even going to be that awesome until we
  ;; teach it how to guess the relevant org heading using message-ids
  ;; from the References or In-Reply-To headers of the incoming
  ;; message.
  (interactive "P")
  (if (not (memq major-mode '(gnus-summary-mode gnus-article-mode)))
      (error "Only works in gnus summary or article mode")
    (org-store-link)
    (let* ((action (if (not arg)
		       gnorb-gnus-message-trigger-default
		     (if (eq gnorb-gnus-message-trigger-default 'todo)
			 'note
		       'todo)))
	   (targ (or heading
		     (org-refile-get-location
		      (format "Trigger heading (%s): " action) nil t))))
      (find-file (nth 1 org-heading))
      (goto-char (nth 3 org-heading))
      (call-interactively
       (if (eq action 'todo)
	   'org-todo
	 'org-add-note)))))

(provide 'gnorb-gnus)
;;; gnorb-gnus.el ends here
