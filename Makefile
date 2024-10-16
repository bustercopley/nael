# GNU Elpa does accept Org files as package documentation:
# https://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/README?id=fa3dd57f9f95cb1acc65582bebc9ac3c5858a095#n199
# But MELPA does not.  That's why we generate the nael.info file by
# ourselves.

# This Makefile depends on:
#   - GNU Emacs
#   - GNU Make
#   - GNU Texinfo
# E.g. when using GNU Guix, you may want to run:
#   $ guix shell emacs make texinfo -- make

SHELL = emacs
.SHELLFLAGS = -Q --batch --eval
.ONESHELL:

manual: README.org
	@(progn
		(require 'ox-texinfo)
		(find-file "README.org")
		(org-texinfo-export-to-info))
