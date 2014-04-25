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
