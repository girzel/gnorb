;;; gnorb-utils.el --- Common utilities for all gnorb stuff.

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

(require 'cl)
(require 'mailcap)
(require 'gnus)
(require 'bbdb)
(require 'org)
(require 'org-bbdb)
(require 'org-gnus)

(mailcap-parse-mimetypes)

(defgroup gnorb nil
  "Glue code between Gnus, Org, and BBDB."
  :tag "Gnorb")

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
    (car recs)))

(defvar gnorb-tmp-dir (make-temp-file "emacs-gnorb" t)
  "Temporary directory where attachments etc are saved.")

;;; see map-y-or-n-p, you idiot

;; (defun gnorb-query-attach (attachments action)
;;   "Run through the attachments, make some queries, do the
;;   action."
;;   (let (continue-switch)
;;     (dolist (a attachments)
;;       (cond ((eq continue-switch 'none)
;; 	     nil)
;; 	    ((eq continue-switch 'all)
;; 	     (funcall action a))
;; 	    (t
;; 	     (let ((input-char
;; 		    (read-char-choice
;; 			(format
;; 			 "Attach %s? (y, n, Y, N): "
;; 			 (file-name-nondirectory a))
;; 			'(?y ?n ?Y ?N))))
;; 	       (cond
;; 		((eq input-char ?y)
;; 		 (funcall action a))
;; 		((equal input-char ?n)
;; 		 nil)
;; 		((equal input-char ?N)
;; 		 (setq continue-switch 'none))
;; 		((equal input-char ?Y)
;; 		 (funcall action a)
;; 		 (setq continue-switch 'all)))))))))

(provide 'gnorb-utils)
;;; gnorb-utils.el ends here
