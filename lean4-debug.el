;;; lean4-debug.el --- Debug mode for lean4-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Code:

(require 'cl-lib)

(defvar lean4-debug-mode nil)

(defvar lean4-debug-buffer-name "*lean4-debug*")

(defun lean4-turn-on-debug-mode (&optional print-msg)
  "Turn on Lean debug.
Print message \"lean: turn on debug mode\" if PRINT-MSG or if called
interactively."
  (interactive)
  (when (or (called-interactively-p 'any) print-msg)
    (message "lean: turn on debug mode"))
  (get-buffer-create lean4-debug-buffer-name)
  (buffer-disable-undo lean4-debug-buffer-name)
  (display-buffer lean4-debug-buffer-name 'display-buffer-reuse-window
                  '((reusable-frames . t)))
  (setq lean4-debug-mode t))

(defun lean4-turn-off-debug-mode (&optional print-msg)
  "Turn off Lean debug.
Print message \"lean: turn off debug mode\" if PRINT-MSG or if called
interactively."
  (interactive)
  (when (eq major-mode 'lean4-mode)
    (when (or (called-interactively-p 'any) print-msg)
      (message "lean: turn off debug mode"))
    (setq lean4-debug-mode nil)))

(defun lean4-output-to-buffer (buffer-name format-string args)
  "Output a message to a buffer.
The buffer is given by BUFFER-NAME.  The message is given by FORMAT-STRING
and ARGS."
  (with-current-buffer
      (get-buffer-create buffer-name)
    (save-selected-window
      (ignore-errors
        (select-window (get-buffer-window buffer-name t)))
      (goto-char (point-max))
      (insert (apply #'format format-string args)))))

(defun lean4-debug (format-string &rest args)
  "Display a message at the bottom of the *lean4-debug* buffer.
The message is given by FORMAT-STRING and ARGS."
  (when lean4-debug-mode
    (let ((time-str (format-time-string "%T.%3N" (current-time))))
      (lean4-output-to-buffer lean4-debug-buffer-name
                             (concat "%s -- " format-string "\n")
                             (cons (propertize time-str 'face 'font-lock-keyword-face)
                                   args)))))

(provide 'lean4-debug)
;;; lean4-debug.el ends here
