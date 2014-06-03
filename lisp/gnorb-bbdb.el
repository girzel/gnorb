;;; gnorb-bbdb.el --- The BBDB-centric functions of gnorb

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

(defgroup gnorb-bbdb nil
  "The BBDB bits of gnorb."
  :tag "Gnorb BBDB"
  :group 'gnorb)


(defcustom gnorb-bbdb-org-tag-field 'org-tags
  "The name (as a symbol) of the field to use for org tags."
  :group 'gnorb-bbdb
  :type 'symbol)

(unless (assoc gnorb-bbdb-org-tag-field bbdb-separator-alist)
  (push `(,gnorb-bbdb-org-tag-field ":" ":") bbdb-separator-alist))

(defcustom gnorb-bbdb-posting-styles nil
  "An alist of styles to use when composing messages to the BBDB
  record(s) under point. This is entirely analogous to
  `gnus-posting-styles', it simply works by examining record
  fields rather than group names.

When composing a message to multiple contacts (using the \"*\"
prefix), the records will be scanned in order, with the record
initially under point (if any) set aside for last. That means
that, in the case of conflicting styles, the record under point
will override the others.

In order not to be too intrusive, this option has no effect on
the usual `bbdb-mail' command. Instead, the wrapper command
`gnorb-bbdb-mail' is provided, which consults this option and
then hands off to `bbdb-compose-mail'. If you'd always like to
use `gnorb-bbdb-mail', you can simply bind it to \"m\" in the
`bbdb-mode-map'.

The value of the option should be a list of sexps, each one
matching a single field. The first element should match a field
name: one of the built-in fields like lastname, or an xfield.
Field names should be given as symbols.

The second element is a regexp used to match against the value of
the field (non-string field values will be cast to strings, if
possible). It can also be a cons of two strings, the first of
which matches the field label, the second the field value.

Alternately, the first element can be the name of a custom
function that is called with the record as its only argument, and
returns either t or nil. In this case, the second element of the
list is disregarded.

All following elements should be field setters for the message to
be composed, just as in `gnus-posting-styles'.

An example value might look like:"
  :group 'gnorb-bbdb)

(defun gnorb-bbdb-mail (records &optional subject n verbose)
  "Acts just like `bbdb-mail', except runs RECORDS through
  `gnorb-bbdb-posting-styles', allowing customization of message
  styles for certain records. From the `bbdb-mail' docstring:

Compose a mail message to RECORDS (optional: using SUBJECT).
Interactively, use BBDB prefix \
\\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If VERBOSE is non-nil (as in interactive calls) be verbose."
  ;; see the function `gnus-configure-posting-styles' for tips on how
  ;; to actually do this.
  (interactive (list (bbdb-do-records) nil
		     (or (consp current-prefix-arg)
                         current-prefix-arg)
		     t))
  (setq records (bbdb-record-list records))
  (if (not records)
      (user-error "No records displayed")
    (let ((head (bbdb-current-record))
	  (to (bbdb-mail-address records n nil verbose))
	  (message-mode-hook (copy-sequence message-mode-hook)))
      (setq records (remove head records))
      (when gnorb-bbdb-posting-styles
	(add-hook 'message-mode-hook
		  `(lambda ()
		     (gnorb-bbdb-configure-posting-styles ,records)
		     (gnorb-bbdb-configure-posting-styles (list ,head)))))
      (bbdb-compose-mail to subject))))

(defun gnorb-bbdb-configure-posting-styles (recs)
  ;; My most magnificent work of copy pasta!
  (dolist (r recs)
    (let (field val label recval element filep
		element v results name address)
      (dolist (style gnorb-bbdb-posting-styles)
	(setq field (pop style)
	      val (pop style))
	(when (consp val)
	  (setq label (pop val)
		val (pop val)))
	(unless (fboundp field)
	  (setq rec-val (bbdb-record-field r field)))
	(when (cond
	       ((eq field 'address)
		(dolist (a rec-val)
		  (unless (and label
			       (not (string-match label (car f))))
		    (string-match val (bbdb-format-address-default f)))))
	       ((eq field 'phone)
		(dolist (p rec-val)
		  (unless (and label
			       (not (string-match label (car f))))
		    (string-match val (bbdb-phone-string p)))))
	       ((consp rec-val)
		(dolist (f rec-val)
		  (string-match var f)))
	       ((fboundp field)
		(funcall field rec))
	       ((stringp rec-val)
		(string-match val rec-val)))
	  (dolist (attribute style)
	    (setq element (pop attribute)
		  filep nil)
	    (setq value
		  (cond
		   ((eq (car attribute) :file)
		    (setq filep t)
		    (cadr attribute))
		   ((eq (car attribute) :value)
		    (cadr attribute))
		   (t
		    (car attribute))))
	    ;; We get the value.
	    (setq v
		  (cond
		   ((stringp value)
		    (if (and (gnus-string-match-p "\\\\[&[:digit:]]" value)
			     (match-beginning 1))
			(gnus-match-substitute-replacement value nil nil group)
		      value))
		   ((or (symbolp value)
			(functionp value))
		    (cond ((functionp value)
			   (funcall value))
			  ((boundp value)
			   (symbol-value value))))
		   ((listp value)
		    (eval value))))
	    ;; Post-processing for the signature posting-style:
	    (and (eq element 'signature) filep
		 message-signature-directory
		 ;; don't actually use the signature directory
		 ;; if message-signature-file contains a path.
		 (not (file-name-directory v))
		 (setq v (nnheader-concat message-signature-directory v)))
	    ;; Get the contents of file elems.
	    (when (and filep v)
	      (setq v (with-temp-buffer
			(insert-file-contents v)
			(buffer-substring
			 (point-min)
			 (progn
			   (goto-char (point-max))
			   (if (zerop (skip-chars-backward "\n"))
			       (point)
			     (1+ (point))))))))
	    (setq results (delq (assoc element results) results))
	    (push (cons element v) results))))
      (setq name (assq 'name results)
	    address (assq 'address results))
      (setq results (delq name (delq address results)))
      (gnus-make-local-hook 'message-setup-hook)
      (setq results (sort results (lambda (x y)
				    (string-lessp (car x) (car y)))))
      (dolist (result results)
	(add-hook 'message-setup-hook
		  (cond
		   ((eq 'eval (car result))
		    'ignore)
		   ((eq 'body (car result))
		    `(lambda ()
		       (save-excursion
			 (message-goto-body)
			 (insert ,(cdr result)))))
		   ((eq 'signature (car result))
		    (set (make-local-variable 'message-signature) nil)
		    (set (make-local-variable 'message-signature-file) nil)
		    (if (not (cdr result))
			'ignore
		      `(lambda ()
			 (save-excursion
			   (let ((message-signature ,(cdr result)))
			     (when message-signature
			       (message-insert-signature)))))))
		   (t
		    (let ((header
			   (if (symbolp (car result))
			       (capitalize (symbol-name (car result)))
			     (car result))))
		      `(lambda ()
			 (save-excursion
			   (message-remove-header ,header)
			   (let ((value ,(cdr result)))
			     (when value
			       (message-goto-eoh)
			       (insert ,header ": " value)
			       (unless (bolp)
				 (insert "\n")))))))))
		  t 'local))
      (when (or name address)
	(add-hook 'message-setup-hook
		  `(lambda ()
		     (set (make-local-variable 'user-mail-address)
			  ,(or (cdr address) user-mail-address))
		     (let ((user-full-name ,(or (cdr name) (user-full-name)))
			   (user-mail-address
			    ,(or (cdr address) user-mail-address)))
		       (save-excursion
			 (message-remove-header "From")
			 (message-goto-eoh)
			 (insert "From: " (message-make-from) "\n"))))
		  t 'local)))))

(defun gnorb-bbdb-tag-agenda (records)
  "Open an Org agenda tags view from the BBDB buffer, using the
value of the record's org-tags field. This shows only TODOs by
default; a prefix argument shows all tagged headings; a \"*\"
prefix operates on all currently visible records. If you want
both, use \"C-u\" before the \"*\"."
  (interactive (list (bbdb-do-records)))
  (require 'org-agenda)
  (unless (and (eq major-mode 'bbdb-mode)
	       (equal (buffer-name) bbdb-buffer-name))
    (error "Only works in the BBDB buffer"))
  (setq records (bbdb-record-list records))
  (let ((tag-string
	 (mapconcat
	  'identity
	  (delete-dups
	   (mapcan (lambda (r)
		     (bbdb-record-xfield-split r gnorb-bbdb-org-tag-field))
		   records))
	  "|")))
    (if tag-string
	;; C-u = all headings, not just todos
	(if (equal current-prefix-arg '(4))
	    (org-tags-view nil tag-string)
	  (org-tags-view t tag-string))
      (error "No org-tags field present"))))

(defun gnorb-bbdb-mail-search (records)
  "Initiate a mail search from the BBDB buffer.

Use the prefix arg to edit the search string first, and the \"*\"
prefix to search mails from all visible contacts. When using both
a prefix arg and \"*\", the prefix arg must come first."
  (interactive (list (bbdb-do-records)))
  (unless (and (eq major-mode 'bbdb-mode)
	       (equal (buffer-name) bbdb-buffer-name))
    (error "Only works in the BBDB buffer"))
  (setq records (bbdb-record-list records))
  (require 'gnorb-gnus)
  (let* ((backend (or (assoc gnorb-gnus-mail-search-backend
			     gnorb-gnus-mail-search-backends)
		      (error "No search backend specified")))
	 (search-string
	  (funcall (second backend)
		   (cl-mapcan 'bbdb-record-mail records))))
    (when (equal current-prefix-arg '(4))
      (setq search-string
	    (read-from-minibuffer
	     (format "%s search string: " (first backend)) search-string)))
    (funcall (third backend) search-string)
    (delete-other-windows)))  

(defun gnorb-bbdb-cite-contact (rec)
  (interactive (list (gnorb-prompt-for-bbdb-record)))
  (let ((mail-string (bbdb-dwim-mail rec)))
   (if (called-interactively-p)
       (insert mail-string)
     mail-string)))

(provide 'gnorb-bbdb)
;;; gnorb-bbdb.el ends here
