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

(defvar gnorb-message-org-ids nil
  "List of Org heading IDs from the outgoing Gnus message, used
  to mark mail TODOs as done once the message is sent."
  ;; The send hook either populates this, or sets it to nil, depending
  ;; on whether the message in question has an Org id header. Then
  ;; `gnorb-org-restore-after-send' checks for it and acts
  ;; appropriately.
)

(provide 'gnorb-utils)
;;; gnorb-utils.el ends here
