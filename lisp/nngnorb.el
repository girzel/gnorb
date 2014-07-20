;;; nngnorb.el --- Gnorb backend for Gnus

;; This file is in the public domain.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net.>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a backend for supporting Gnorb-related stuff. I'm going to
;; regret this, I know.

;; It started off just with wanting to collect all the gnus links in a
;; subtree, and display all the messages in an ephemeral group. But it
;; doesn't seem possible to create ephemeral groups without
;; associating them with a server, and which server would that be?
;; Nnir also provides a nice interface to creating ephemeral groups,
;; but again, it relies on a server parameter to know which nnir
;; engine to use, and if you try to fake it it still craps out.

;; So this file is a copy-pasta from nnnil.el -- I'm trying to keep
;; this as simple as possible. Right now it does nothing but serving
;; as a place to hang ephemeral groups made with nnir searches of
;; message from the rest of your gnus installation. Enjoy.

;;; Code:

(eval-and-compile
  (require 'nnheader)
  (require 'nnir))

(defvar nngnorb-status-string "")

(gnus-declare-backend "nngnorb" 'none)

(add-to-list 'nnir-method-default-engines '(nngnorb . gnorb))

(add-to-list 'nnir-engines
	     '(gnorb nnir-run-gnorb))

(defun nnir-run-gnorb (query server &optional group)
  "Run the actual search for messages to display. See nnir.el for
some details of how this gets called.

As things stand, the query string can be given as one of two
different things. First is the ID string of an Org heading,
prefixed with \"id+\". This was probably a bad choice as it could
conceivably look like an org tags search string. Fix that later.
If it's an ID, then the entire subtree text of that heading is
scanned for gnus links, and all the linked messages are displayed
in an ephemeral group.

Otherwise, the query string can be a tags match string, a la the
Org agenda tags search. All headings matched by this string will
be scanned for gnus messages, and those messages displayed."
  (save-excursion
    (let ((q (cdr (assq 'query query)))
	  (buf (get-buffer-create nnir-tmp-buffer))
	  links vectors)
      (with-current-buffer buf
	(erase-buffer))
      (when (equal "5.13" gnus-version-number)
	(setq q (car q)))
      (cond ((string-match "id\\+\\([[:alnum:]-]+\\)$" q)
	     (with-demoted-errors "Error: %S"
	       (org-id-goto (match-string 1 q))
	       (append-to-buffer
		buf
		(point)
		(org-element-property
		 :end (org-element-at-point)))))
	    ((listp q)
	     ;; be a little careful: this could be a list of links, or
	     ;; it could be the full plist
	     (setq links (if (plist-member q :gnus)
			     (plist-get q :gnus)
			   q)))
	    (t (org-map-entries
		(lambda ()
		  (append-to-buffer
		   buf
		   (point)
		   (save-excursion
		     (outline-next-heading)
		     (point))))
		q
		'agenda)))
      (with-current-buffer buf
	(let (ids)
	  (goto-char (point-min))
	  (setq links (plist-get (gnorb-scan-links (point-max) 'gnus)
				 :gnus))
	  (goto-char (point-min))
	  (while (re-search-forward
		  (concat ":" gnorb-org-msg-id-key ": \\([^\n]+\\)")
		  (point-max) t)
	    (setq ids (append (split-string (match-string 1)) ids)))
	  (when ids
	    (dolist (id ids)
	      (let ((link (gnorb-msg-id-to-link id)))
		(when link
		  (push link links)))))))
      (setq links (delete-dups links))
      (unless (gnus-alive-p)
	(gnus))
      (dolist (m links (when vectors
			 (nreverse vectors)))
	(let (server-group msg-id result artno)
	  (setq m (org-link-unescape m))
	  (when (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" m)
	    (setq server-group (match-string 1 m)
		  msg-id (match-string 3 m)
		  result (ignore-errors (gnus-request-head msg-id server-group)))
	    (when result
	     (setq artno (cdr result))
	     (when (and (integerp artno) (> artno 0))
	       (push (vector server-group artno 100) vectors)))))))))

(defvar nngnorb-status-string "")

(defun nngnorb-retrieve-headers (articles &optional group server fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  'nov)

(defun nngnorb-open-server (server &optional definitions)
  t)

(defun nngnorb-close-server (&optional server)
  t)

(defun nngnorb-request-close ()
  t)

(defun nngnorb-server-opened (&optional server)
  t)

(defun nngnorb-status-message (&optional server)
  nngnorb-status-string)

(defun nngnorb-request-article (article &optional group server to-buffer)
  (setq nngnorb-status-string "No such group")
  nil)

(defun nngnorb-request-group (group &optional server fast info)
  (let (deactivate-mark)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (insert "411 no such news group\n")))
  (setq nngnorb-status-string "No such group")
  nil)

(defun nngnorb-close-group (group &optional server)
  t)

(defun nngnorb-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  t)

(defun nngnorb-request-post (&optional server)
  (setq nngnorb-status-string "Read-only server")
  nil)

(provide 'nngnorb)

;;; nnnil.el ends here
