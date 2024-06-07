;;; lean4-mode.el --- A major mode for the Lean language  -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author:
;;   Adam Topaz <topaz@ualberta.ca>
;;   Akira Komamura <akira.komamura@gmail.com>
;;   Bao Zhiyuan
;;   Daniel Selsam <daniel.selsam@protonmail.com>
;;   Gabriel Ebner <gebner@gebner.org>
;;   Henrik Böving <hargonix@gmail.com>
;;   Hongyu Ouyang
;;   Jakub Bartczuk <bartczukkuba@gmail.com>
;;   Leonardo de Moura <leonardo@microsoft.com>
;;   Mauricio Collares <mauricio@collares.org>
;;   Mekeor Melire <mekeor@posteo.de>
;;   Philip Kaludercic <philipk@posteo.net>
;;   Richard Copley <buster@buster.me.uk>
;;   Sebastian Ullrich <sebasti@nullri.ch>
;;   Siddharth Bhat <siddu.druid@gmail.com>
;;   Simon Hudon <simon.hudon@gmail.com>
;;   Soonho Kong <soonhok@cs.cmu.edu>
;;   Tomáš Skřivan <skrivantomas@seznam.cz>
;;   Wojciech Nawrocki <wjnawrocki@protonmail.com>
;;   Yael Dillies <yael.dillies@gmail.com>
;;   Yury G. Kudryashov <urkud@urkud.name>
;; Homepage: https://codeberg.org/mekeor/emacs-lean
;; Keywords: languages
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: ((emacs "29.1")
;;                    (magit-section "2.90.1")
;;                    (eglot "1.15")
;;                    (markdown-mode "2.6"))
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; Provides a major mode for the Lean programming language.

;; Provides highlighting, diagnostics, goal visualization,
;; and many other useful features for Lean users.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'flymake)
(require 'flymake-proc)
(require 'pcase)
(require 'quail)

(require 'markdown-mode)

(require 'lean4-info)
(require 'lean4-settings)
(require 'lean4-syntax)
(require 'lean4-util)

(defun lean4-compile-string (lake-name exe-name args file-name)
  "Command to run EXE-NAME with extra ARGS and FILE-NAME.

If LAKE-NAME is nonempty, then prepend \"LAKE-NAME env\" to the
command \"EXE-NAME ARGS FILE-NAME\"."
  (if lake-name
      (format "%s env %s %s %s" lake-name exe-name args file-name)
      (format "%s %s %s" exe-name args file-name)))

(defun lean4-create-temp-in-system-tempdir (file-name prefix)
  "Create a temp lean file and return its name.

The new file has prefix PREFIX (defaults to `flymake') and the same
extension as FILE-NAME."
  (make-temp-file
   (or prefix "flymake") nil (file-name-extension file-name)))

;; TODO: User should make this evaluate themselves explicitely.
(add-to-list 'project-vc-extra-root-markers "lakefile.lean")

(defun lean4-lake-build ()
  "Call lake build."
  (interactive)
  (if-let ((default-directory (project-root (project-current))))
      (compile "lake build")))

(defun lean4-execute (&optional arg)
  "Execute Lean in the current buffer with an optional argument ARG."
  (interactive)
  (when (called-interactively-p 'any)
    (setq arg (read-string "arg: " arg)))
  (let*
      ((root (project-root (project-current)))
       (use-lake
        (file-exists-p (file-name-concat root "lakefile.lean")))
       (default-directory (if use-lake root default-directory))
       (target-file-name
        (or
         (buffer-file-name)
         (flymake-proc-init-create-temp-buffer-copy
          #'lean4-create-temp-in-system-tempdir))))
    (compile
     (lean4-compile-string
	  (and use-lake "lake")
      "lean"
      (or arg "")
      (shell-quote-argument (expand-file-name target-file-name))))))

(defun lean4-refresh-file-dependencies ()
  "Refresh the file dependencies.

This function restarts the server subprocess for the current
file, recompiling, and reloading all imports."
  (interactive)
  (when eglot--managed-mode
    (eglot--signal-textDocument/didClose)
    (eglot--signal-textDocument/didOpen)))

(define-abbrev-table 'lean4-abbrev-table nil)

(defvar-keymap lean4-mode-map
  :parent prog-mode-map
  "C-c C-x" #'lean4-execute
  "C-c C-l" #'lean4-execute
  "C-c C-k" #'quail-show-key
  "C-c C-i" #'lean4-toggle-info
  "C-c C-b" #'lean4-lake-build
  "C-c C-d" #'lean4-refresh-file-dependencies)

(easy-menu-define lean4-mode-menu lean4-mode-map
  "Menu for the Lean major mode."
  '("Lean 4"
    ["Execute lean"         lean4-execute           t]
    ["Toggle info display"  lean4-toggle-info       t]
    ["Restart lean process" eglot-reconnect         t]
    ["Customize lean4-mode" (customize-group 'lean) t]))

(defvar lean4--idle-timer nil)
(defvar lean4--idle-buffer nil)
(defvar lean4--idle-tick nil)

(defun lean4--idle-invalidate ()
  "Cause `lean4--idle-function' to act as if something has changed.

...when next run."
  (setq lean4--idle-buffer nil))

(defun lean4--idle-function ()
  (when (eq major-mode 'lean4-mode)
    (let ((old-buffer lean4--idle-buffer)
          (old-tick lean4--idle-tick))
      (setq lean4--idle-buffer (current-buffer))
      (setq lean4--idle-tick (buffer-modified-tick))
      ;; If the user has switched buffer or the buffer is not
      ;; modified, refresh the info buffer now. Otherwise (if the
      ;; buffer is modified), do nothing: we will refresh the info
      ;; buffer later, from Eglot's internal
      ;; `eglot--document-changed-hook'.
      (when (or (not (eq lean4--idle-buffer old-buffer))
                (eq lean4--idle-tick old-tick))
        (lean4-info-buffer-refresh)))))

(defun lean4--start-idle-timer ()
  (unless lean4--idle-timer
    (setq lean4--idle-timer
          (run-with-idle-timer lean4-idle-delay t
                               #'lean4--idle-function))))

(defun lean4--cancel-idle-timer ()
  (when lean4--idle-timer
    (cancel-timer lean4--idle-timer)
    (setq lean4--idle-timer nil)))

(lean4--start-idle-timer)

;;;###autoload
(define-derived-mode lean4-mode prog-mode "Lean 4"
  "Major mode for Lean.
\\{lean4-mode-map}
Invokes `lean4-mode-hook'."
  :syntax-table lean4-syntax-table
  :abbrev-table lean4-abbrev-table
  :group 'lean
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-start-skip) "[-/]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip)
       "[ \t]*\\(-/\\|\\s>\\)")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'font-lock-defaults)
       lean4-font-lock-defaults)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set 'compilation-mode-font-lock-keywords '())
  (require 'lean4-input)
  (set-input-method "Lean")
  ;; Inhibit flymake from starting automatically. Since diagnostics
  ;; are updated only by the language server, we call `flymake-start'
  ;; on their receipt.
  (setq-local flymake-no-changes-timeout nil)
  (setq-local flymake-start-on-flymake-mode nil)
  (setq-local flymake-start-on-save-buffer nil)
  ;; Let the `next-error' and `previous-error' commands navigate
  ;; diagnostics.
  (setq-local next-error-function 'flymake-goto-next-error))

(defun lean4--version ()
  "Return Lean version as a list `(MAJOR MINOR PATCH)'."
  (with-temp-buffer
    (call-process "lean" nil (list t nil) nil "-v")
    (goto-char (point-min))
    (re-search-forward
     (rx bol "Lean (version " (group (+ digit) (+ "." (+ digit)))))
    (version-to-list (match-string 1))))

(defun lean4-show-version ()
  "Print Lean 4 version."
  (interactive)
  (message "Lean %s"
           (mapconcat #'number-to-string (lean4--version) ".")))

;;;###autoload
(setf (alist-get "\\.lean\\'" auto-mode-alist) 'lean4-mode)

;;;###autoload
(setf (alist-get "lean" markdown-code-lang-modes) 'lean4-mode)

(setf (alist-get lean4-mode eglot-server-programs)
      '(lean4-eglot-lsp-server . ("lake" "serve")))

(defclass lean4-eglot-lsp-server (eglot-lsp-server) nil
  :documentation "Eglot LSP server subclass for the Lean 4 server.")

;; We call `lean4-info-buffer-refresh' and `flymake-start' from a
;; timer to reduce nesting of synchronous json requests, with a
;; (short) nonzero delay in case the server sends diagnostics
;; excessively frequently.
(defvar lean4--diagnostics-pending nil)
(defun lean4--handle-diagnostics (server uri)
  (setq lean4--diagnostics-pending nil)
  (lean4-with-uri-buffers server uri
    (lean4-info-buffer-refresh)
    (flymake-start)))

(cl-defmethod eglot-handle-notification
  :after
  ((server lean4-eglot-lsp-server)
   (_ (eql textDocument/publishDiagnostics))
   &key uri
   &allow-other-keys)
  "Handle notification textDocument/publishDiagnostics."
  (unless lean4--diagnostics-pending
    (setq lean4--diagnostics-pending t)
    (run-with-timer 0.05 nil #'lean4--handle-diagnostics server uri)))

(cl-defmethod eglot-register-capability
  ((_server lean4-eglot-lsp-server)
   (_method (eql workspace/didChangeWatchedFiles))
   _id &key _watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles."
  (when lean4-enable-file-watchers
    (cl-call-next-method)))

(cl-defmethod eglot-unregister-capability
  ((_server lean4-eglot-lsp-server)
   (_method (eql workspace/didChangeWatchedFiles))
   _id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles."
  (when lean4-enable-file-watchers
    (cl-call-next-method)))

;; Workarounds for code actions, see
;;   https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66552
;;   https://github.com/leanprover/lean4/pull/2721
(defun lean4-mode--before--eglot-read-execute-code-action (args)
  "Before advicing `eglot--read-execute-code-action' for Lean server.

For `Try this' quickfixes, append the new text to the title, so the
user knows which item is which."
  (when (eq (type-of (eglot-current-server)) 'lean4-eglot-lsp-server)
    (dolist (action (car args))
      (eglot--dbind ((CodeAction) edit) action
        (when edit
          (eglot--dbind ((WorkspaceEdit) documentChanges) edit
            ;; Eglot cannot handle duplicate titles in a list of code
            ;; actions because the title is used as a key in the alist
            ;; passed to `completing-read'. To disambiguate (and
            ;; incidentally, let the user know which action is which),
            ;; append the newText to the title. Do this only if the
            ;; original title is "Apply 'Try this'".  Currently only
            ;; the "Try this" quickfixes from Mathlib library search
            ;; tactics (exact?, apply?, rw?) are sent as a list of
            ;; code actions with identical titles.
            (let* ((title (cl-getf action :title))
                   (change0 (aref documentChanges 0))
                   (edit0 (aref (cl-getf change0 :edits) 0))
                   (newText (cl-getf edit0 :newText)))
              (when (string= title "Apply 'Try this'")
                (setf (cl-getf action :title)
                      (concat title ": " newText)))))))))
    args)

(advice-add 'eglot--read-execute-code-action :filter-args
            #'lean4-mode--before--eglot-read-execute-code-action)

(cl-defmethod eglot-execute
  :before ((_server lean4-eglot-lsp-server) action)
  "Massage a `CodeAction' before Eglot handles it.

If ACTION is a fully resolved `CodeAction' (that is, if it contains
edits) and if any text document version number is zero, set it to nil
to tell Eglot not to validate the version."
  (eglot--dcase action
    (((CodeAction) edit)
     (when edit
       (eglot--dbind ((WorkspaceEdit) documentChanges) edit
         ;; Set each document version to nil if it is zero
         (mapc (eglot--lambda ((TextDocumentEdit) textDocument)
                 (when (eq (cl-getf textDocument :version) 0)
                   (setf (cl-getf textDocument :version) nil)))
               documentChanges))))))

(provide 'lean4-mode)

;;; lean4-mode.el ends here
