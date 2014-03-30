gnorb
=====

Glue code between the Gnus, Org, and BBDB packages for Emacs.

"Converging on Microsoft Office"

The purpose of this package is to connect Emacs-based email, project
management, and contact management a little more closely together.

Code in this package is aimed at the development (git) versions of
Gnus, Org, and BBDB. I'll try to make it work with the most recent
stable releases of those packages, but I'm not promising anything.

Put "gnorb/lisp" in your load path, then either require "gnorb" to
load everything, or pick bits and pieces: "gnorb-gnus", "gnorb-org",
or "gnorb-bbdb".

gnorb-bbdb
----------

Current functions include:

* gnorb-bbdb-tag-agenda: Give BBDB records an org-tag field
  (customizable), then call this function on the current records(s) to
  open an Org agenda tags search using those tags.
* gnorb-bbdb-mail-search: Call with current records(s) to search for
  all mail messages from those records. Currently only implemented for
  the notmuch search backend.
