;;; lean4-dev.el --- Development commands for lean4-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation.  All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; This library currently provides `lean4-diff-test-file' command.

;;; Code:

(require 'lean4-util)

(defun lean4-diff-test-file ()
  "Use interactive ./test_input.sh on file of current buffer."
  (interactive)
  (save-buffer)
  ;; yes: auto-agree to copying missing files
  (message
   (shell-command-to-string
    (format
     (concat "yes | PATH=%s/bin:$PATH LEAN_NIX_ARGS=--quiet "
             "./test_single.sh -i \"%s\"")
     (lean4-get-rootdir)
     (file-name-nondirectory (buffer-file-name))))))

(provide 'lean4-dev)

;;; lean4-dev.el ends here
