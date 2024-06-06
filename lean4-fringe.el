;;; lean4-fringe.el --- Show Lean processing progress in the editor fringe -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; Show Lean processing progress in the editor fringe.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'lean4-settings)
(require 'lean4-util)

(defvar-local lean4-fringe-delay-timer nil)

(defface lean4-fringe-face
  nil
  "Face to highlight Lean file progress."
  :group 'lean4)

(if (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'lean4-fringe-fringe-bitmap
    (vector) 16 8))

(defface lean4-fringe-fringe-processing-face
  '((((class color) (background light))
     :background "chocolate1")
    (((class color) (background dark))
     :background "navajo white")
    (t :inverse-video t))
  "Face to highlight the fringe of Lean file processing progress."
  :group 'lean)

(defface lean4-fringe-fringe-fatal-error-face
  '((((class color) (background light))
     :background "red")
    (((class color) (background dark))
     :background "red")
    (t :inverse-video t))
  "Face to highlight the fringe of Lean file fatal errors."
  :group 'lean)

(defun lean4-fringe-fringe-face (lean-file-progress-processing-info)
  (let ((kind (cl-getf lean-file-progress-processing-info :kind)))
    (cond
     ((eq kind 1) 'lean4-fringe-fringe-processing-face)
     (t 'lean4-fringe-fringe-fatal-error-face))))

(defvar-local lean4-fringe-data nil)

(defun lean4-fringe-update-progress-overlays ()
  "Update processing bars in the current buffer."
  (dolist (ov (flatten-tree (overlay-lists)))
    (when (eq (overlay-get ov 'face) 'lean4-fringe-face)
      (delete-overlay ov)))
  (when lean4-show-file-progress
    (seq-doseq (item lean4-fringe-data)
      (let* ((reg (eglot--range-region (cl-getf item :range)))
             (ov (make-overlay (car reg) (cdr reg))))
        (overlay-put ov 'face 'lean4-fringe-face)
        (overlay-put
         ov 'line-prefix
         (propertize
          " " 'display
          `(left-fringe
            lean4-fringe-fringe-bitmap
            ,(lean4-fringe-fringe-face item))))
        (overlay-put ov 'help-echo (format "processing..."))))))

(defvar-local lean4-fringe-delay-timer nil)

(defun lean4-fringe-update (server processing uri)
  (lean4-with-uri-buffers server uri
    (setq lean4-fringe-data processing)
    (unless (and lean4-fringe-delay-timer
                 (memq lean4-fringe-delay-timer timer-list))
      (setq lean4-fringe-delay-timer
            (run-at-time
             0.3 nil
             (lambda (buf)
               (with-current-buffer buf
                 (lean4-fringe-update-progress-overlays)
                 (setq lean4-fringe-delay-timer nil)))
             (current-buffer))))))

(provide 'lean4-fringe)
;;; lean4-fringe.el ends here
