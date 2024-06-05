;;; lean4-lake.el --- Lake integration for lean4-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; This library provides integration with Lake, Lean 4 build system
;; and package manager.

;;; Code:

(require 'lean4-util)
(require 'lean4-settings)

(defun lean4-lake-find-dir-in (dir)
  "Find a parent directory of DIR with file \"lakefile.lean\"."
  (when dir
    (or (when (file-exists-p (expand-file-name "lakefile.lean" dir)) dir)
	    (lean4-lake-find-dir-in (file-name-directory (directory-file-name dir))))))

(defun lean4-lake-find-dir ()
  "Find a parent directory of the current file with file \"lakefile.lean\"."
  (and (buffer-file-name)
       (lean4-lake-find-dir-in (directory-file-name (buffer-file-name)))))

(defun lean4-lake-find-dir-safe ()
  "Call `lean4-lake-find-dir', error on failure."
  (or (lean4-lake-find-dir)
      (error "Cannot find lakefile.lean for %s" (buffer-file-name))))

(defun lean4-lake-build ()
  "Call lake build."
  (interactive)
  (let ((default-directory (file-name-as-directory (lean4-lake-find-dir-safe))))
    (compile (concat (lean4-get-executable lean4-lake-name) " build"))))

(provide 'lean4-lake)
;;; lean4-lake.el ends here
