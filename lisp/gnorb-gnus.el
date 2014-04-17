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

;;; What follows is a very careful copy-pasta of bits and pieces from
;;; mm-decode.el and gnus-art.el. Voodoo was involved.

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
  (let ((filename (or (mail-content-type-get
			(mm-handle-disposition handle) 'filename)
		       (mail-content-type-get
			(mm-handle-type handle) 'name)))
	(org-heading (or org-heading
			 (org-refile-get-location "Attach part to"))))
    (require 'org-attach)
    (when filename
      (setq filename (gnus-map-function mm-file-name-rewrite-functions
					(file-name-nondirectory filename))))
    ;; Get a temp pathname inside `gnorb-tmp-dir', and save the
    ;; attachment there
    (setq filename (expand-file-name filename gnorb-tmp-dir))
    (mm-save-part-to-file handle filename)
    ;; then visit the headline in question...
    (save-window-excursion
      (find-file (nth 1 org-heading))
      (goto-char (nth 3 org-heading))
     ;; ...and actually attach the file, moving it out of the tmp dir
     (org-attach-attach filename nil 'mv))))

;;; Something is still slightly wrong about the following -- it
;;; doesn't provide "a" as a key on the button itself, which is what I
;;; was hoping.

;; (gnus-define-keys (gnus-summary-mime-map "K" gnus-summary-mode-map)
;;   "a" gnorb-gnus-article-org-attach)

;; (push '("attach to org heading" . gnorb-gnus-mime-org-attach)
;;       gnus-mime-action-alist)

;; (push '(gnorb-gnus-mime-org-attach "a" "Attach to Org heading")
;;       gnus-mime-button-commands)

(provide 'gnorb-gnus)
;;; gnorb-gnus.el ends here
