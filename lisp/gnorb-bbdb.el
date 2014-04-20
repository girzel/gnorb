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

(defun gnorb-bbdb-tag-agenda (records)
  "Open an Org agenda tags view from the BBDB buffer, using the
value of the record's org-tags field. A prefix argument limits to
TODOs only; a \"*\" prefix operates on all currently visible
records. If you want both, use \"C-u\" before the \"*\"."
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
	;; C-u = todos only
	(if (equal current-prefix-arg '(4))
	    (org-tags-view t tag-string)
	  (org-tags-view nil tag-string))
      (error "No org-tags field present"))))

(defcustom gnorb-bbdb-mail-search-backends
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
  :group 'gnorb-bbdb
  :type 'list)

(defcustom gnorb-bbdb-mail-search-backend nil
  "Mail search backend currently in use."
  :group 'gnorb-bbdb
  :type 'symbol)

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
  (let* ((backend (or (assoc gnorb-bbdb-mail-search-backend
			     gnorb-bbdb-mail-search-backends)
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