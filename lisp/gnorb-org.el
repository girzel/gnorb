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

(defun gnorb-org-contact-link (rec)
  (interactive (list (gnorb-prompt-for-bbdb-record)))
  (let* ((name (bbdb-record-name rec))
	 (link (concat "bbdb:" (org-link-escape name))))
    (org-store-link-props :type "bbdb" :name name
			  :link link :description name)
    (if (called-interactively-p)
	(insert (format "[[%s][%s]]" link name))
      link)))

;; (eval-after-load "gnorb-org"
;;   '(progn
;;      (global-set-key (kbd "C-c C") 'gnorb-bbdb-cite-contact)))


(provide 'gnorb-org)
;;; gnorb-org.el ends here
