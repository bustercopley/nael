;;; nael-util.el --- Utilities for nael-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; This library provides utilities for `nael-mode'.

;;; Code:

(require 'cl-lib)

(defun nael-line-offset (&optional pos)
  "Return the byte-offset of POS or current position.

Counts from the beginning of the line."
  (interactive)
  (let* ((pos (or pos (point)))
         (bol-pos
          (save-excursion
            (goto-char pos)
            (beginning-of-line)
            (point))))
    (- pos bol-pos)))

(defun nael-pos-at-line-col (l c)
  "Return the point of the given line L and column C."
  ;; http://emacs.stackexchange.com/a/8083
  (save-excursion
    (goto-char (point-min))
    (forward-line (- l 1))
    (move-to-column c)
    (point)))

(defun nael-in-comment-p ()
  "Return t iff a current point is inside of comment block.

Return nil otherwise."
  (nth 4 (syntax-ppss)))

;; The following function is a slightly modified version of
;; `f--collect-entries' written by Johan Andersson accessible at
;; https://github.com/rejeep/f.el/blob/master/f.el#L416-L435
(defun nael--collect-entries (path recursive)
  "Find all files in PATH.  If RECURSIVE, descend into subfolders.

This is a modified version of `f--collect-entries' that waits for
0.0001s before descending into subfolders.  This allows `wait-timeout'
function to check the timer and kill the execution of this function."
  (let (result
        (entries
         (seq-remove
          (lambda (file)
            (or
             (equal (file-name-nondirectory file) ".")
             (equal (file-name-nondirectory file) "..")))
          (directory-files path t))))
    ;; The following line is the only modification that I made
    ;; It waits 0.0001 second for an event. This wait allows
    ;; wait-timeout function to check the timer and kill the execution
    ;; of this function.
    (sit-for 0.0001)
    (cond (recursive
           (mapc
            (lambda (entry)
              (if (file-regular-p entry)
                  (setq result (cons entry result))
                (when (file-directory-p entry)
                  (setq result (cons entry result))
                  (setq result
                        (append
                         result
                         (nael--collect-entries entry recursive))))))
            entries))
          (t (setq result entries)))
    result))

;; The following function is a slightly modified version of
;; f-files function written by Johan Andersson The URL is at
;; https://github.com/rejeep/f.el/blob/master/f.el#L478-L481
(defun nael-find-files (path &optional fn recursive)
  "Find all files in PATH.

Optionally filter files satisfying predicate FN and/or use RECURSIVE
search."
  ;; It calls `nael--collect-entries' instead of `f--collect-entries'
  (let ((files (seq-keep #'file-regular-p
                         (nael--collect-entries path recursive))))
    (if fn (seq-keep fn files) files)))

(defmacro nael-with-uri-buffers (server uri &rest body)
  (declare (indent 2)
           (debug (form form &rest form)))
  (let ((path-var (make-symbol "path")))
    `(let ((,path-var (abbreviate-file-name
                       (file-truename
                        (eglot-uri-to-path ,uri)))))
       (dolist (buf (eglot--managed-buffers ,server))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (and buffer-file-name
                        (string= buffer-file-truename ,path-var))
               ,@body)))))))

(provide 'nael-util)

;;; nael-util.el ends here
