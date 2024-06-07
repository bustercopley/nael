;;; lean4-settings.el --- Custom variables for lean4-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; This library defines custom variables for `lean4-mode'.

;;; Code:

(require 'cl-lib)

(defgroup lean4 nil
  "Lean 4 programming language and theorem prover."
  :prefix "lean4-"
  :group 'languages)

(defgroup lean4-keybinding nil
  "Keybindings for lean4-mode."
  :prefix "lean4-"
  :group 'lean)

(defcustom lean4-highlight-inaccessible-names t
  "Use font to highlight inaccessible names.

Set this variable to `t' to highlight inaccessible names in the info
display using `font-lock-comment-face' instead of the `✝' suffix used
by Lean."
  :group 'lean
  :type 'boolean)

(defcustom lean4-idle-delay 0.3
  "Interval for `lean4-idle-hook' functions."
  :type 'number
  :group 'lean4-mode)

(defcustom lean4-enable-file-watchers nil
  "Honour requests from the server to watch for file modifications.

This is disabled by default because the server wants to watch
`**/*.ilean', and in many cases there are too many directories to
watch each individually."
  :group 'lean4
  :type 'boolean)

(provide 'lean4-settings)

;;; lean4-settings.el ends here
