* Gnorb

Glue code between the Gnus, Org, and BBDB packages for Emacs.

This package connects Emacs-based email, project management, and
contact management a little more closely together. The goal is to
reduce friction when manipulating TODOs, contacts, messages, and
files.

Probably the most interesting thing Gnorb does is tracking
correspondences between Gnus email messages and Org headings. Rather
than "turning your inbox into a TODO list", as some software puts it,
Gnorb (kind of) does the opposite: turning your TODO headings into
mini mailboxes.

*Note for previous users*: If you were using Gnorb from Github before
it shifted to the Elpa repository, the email tracking mechanism has
changed, please see the manual for details.

** Installation

It's easiest to install Gnorb from Elpa: run `list-packages' and look
for it there.

Or clone the Git repo at https://github.com/girzel/gnorb, and add the
top-level directory to your load path.

If you want to use Gnorb for tracking emails with TODOs, you'll need
to add a nngnorb server to your `gnus-secondary-select-methods'
variable, then call `gnorb-tracking-initialize' in your init files.
Again, see the manual for details.
