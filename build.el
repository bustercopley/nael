#!/usr/bin/env -S emacs -Q --script

;; Why this script?  GNU Elpa accepts Org files as package
;; documentation¹ but MELPA does not.  That's why we generate the
;; nael.info and nael.texi files by ourselves.
;;
;; [1]: https://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/README
;;      Section "** =:doc FILE="

(require 'ox-texinfo)
(find-file "README.org")
(org-texinfo-export-to-info)
