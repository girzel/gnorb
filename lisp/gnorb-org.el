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

(defvar gnorb-org-window-conf nil
  "Save org-buffer window configuration here, for restoration
  after the mail is sent.")

(defun gnorb-org-restore-after-send ()
  (gnus-summary-exit nil t)
  (when (window-configuration-p gnorb-org-window-conf)
    (set-window-configuration gnorb-org-window-conf))
  ; Should check here that we actually made it back to the right org
  ; heading. Could save an ID prop on the heading and check for that.
  (call-interactively 'org-agenda-todo))

(defun gnorb-org-handle-mail (&optional from-agenda)
  "Handle mail-related links for current headline."
  (interactive)
  (unless (org-back-to-heading t)
    (error "Not in an org item"))
  (unless from-agenda
    ;; window conf should return to the agenda.
    (setq gnorb-org-window-conf (current-window-configuration)))
  (let (message mailto)
    (while (re-search-forward org-any-link-re (line-end-position) t)
      (let ((addr (or (match-string-no-properties 2)
		      (match-string-no-properties 0))))
	(cond
	 ((string-match "^<?gnus:" addr)
	  (push (substring addr (match-end 0)) message))
	 ((string-match "^<?mailto:" addr)
	  (push (substring addr (match-end 0)) mailto))
	 ((string-match-p "^<?bbdb:" addr)
	  (with-current-buffer bbdb-buffer-name
	    (let ((recs bbdb-records))
	      (org-open-link-from-string addr)
	      (let ((mail (bbdb-mail-address (bbdb-current-record))))
		(bbdb-display-records recs)
		(push mail mailto))))))))
    (cond
     (message
      (org-gnus-open (org-link-unescape (car message)))
      (call-interactively
       'gnus-summary-wide-reply-with-original)
      (when mailto
	(message-goto-to)
	(insert ", ")
	(insert (mapconcat 'identity mailto ", ")))
      (add-to-list 'message-exit-actions
		   'gnorb-org-restore-after-send t)
      (message-goto-body))
     (mailto
      (compose-mail (mapconcat 'identity mailto ", ")
		    nil nil nil nil nil nil
		    'gnorb-org-restore-after-send))
     (t
      (error "No mail-related links in headline")))))

(defun gnorb-org-handle-mail-agenda ()
  "Examine item at point for mail-related links, and handle them."
  (interactive)
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (setq gnorb-org-window-conf (current-window-configuration))
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (gnorb-org-handle-mail t))))

;; (eval-after-load "gnorb-org"
;;   '(progn
;;      (global-set-key (kbd "C-c C") 'gnorb-bbdb-cite-contact)
;;      (global-set-key (kbd "C-c H") 'gnorb-org-handle-mail)))
;; (eval-after-load 'org-agenda
;;   '(org-defkey org-agenda-mode-map (kbd "H") 'gnorb-org-handle-mail-agenda))

(provide 'gnorb-org)
;;; gnorb-org.el ends here
