;;; nael-mode.el --- A major mode for the Lean language  -*- lexical-binding: t; -*-

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

(require 'nael-input)
(require 'nael-info)
(require 'nael-syntax)
(require 'nael-util)

(defgroup nael nil
  "Lean programming language and theorem prover."
  :prefix "nael-"
  :group 'languages)

(defcustom nael-idle-delay 0.3
  "Interval for `nael-idle-hook' functions."
  :group 'nael :type 'number)

(defcustom nael-enable-file-watchers nil
  "Honour requests from the server to watch for file modifications.

This is disabled by default because the server wants to watch
`**/*.ilean', and in many cases there are too many directories to
watch each individually."
  :group 'nael :type 'boolean)

(defun nael-compile-string (lake-name exe-name args file-name)
  "Command to run EXE-NAME with extra ARGS and FILE-NAME.

If LAKE-NAME is nonempty, then prepend \"LAKE-NAME env\" to the
command \"EXE-NAME ARGS FILE-NAME\"."
  (if lake-name
      (format "%s env %s %s %s" lake-name exe-name args file-name)
      (format "%s %s %s" exe-name args file-name)))

(defun nael-create-temp-in-system-tempdir (file-name prefix)
  "Create a temp lean file and return its name.

The new file has prefix PREFIX (defaults to `flymake') and the same
extension as FILE-NAME."
  (make-temp-file
   (or prefix "flymake") nil (file-name-extension file-name)))

;; TODO: User should make this evaluate themselves explicitely.
(add-to-list 'project-vc-extra-root-markers "lakefile.lean")

(defun nael-lake-build ()
  "Call lake build."
  (interactive)
  (if-let ((default-directory (project-root (project-current))))
      (compile "lake build")))

(defun nael-execute (&optional arg)
  "Execute Lean in the current buffer with an optional argument ARG."
  (interactive)
  ;; TODO: Move this inside the (interactive ...) form.
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
          #'nael-create-temp-in-system-tempdir))))
    (compile
     (nael-compile-string
	  (and use-lake "lake")
      "lean"
      (or arg "")
      (shell-quote-argument (expand-file-name target-file-name))))))

(defvar-keymap nael-mode-map
  :parent prog-mode-map
  "C-c C-x" #'nael-execute
  "C-c C-l" #'nael-execute
  "C-c C-k" #'quail-show-key
  "C-c C-i" #'nael-toggle-info
  "C-c C-b" #'nael-lake-build)

(easy-menu-define nael-mode-menu nael-mode-map
  "Menu for the Lean major mode."
  '("Nael"
    ["Execute lean"         nael-execute           t]
    ["Toggle info display"  nael-toggle-info       t]
    ["Restart lean process" eglot-reconnect         t]
    ["Customize nael-mode" (customize-group 'lean) t]))

(defvar nael--idle-timer nil)
(defvar nael--idle-buffer nil)
(defvar nael--idle-tick nil)

(defun nael--idle-invalidate ()
  "Cause `nael--idle-function' to act as if something has changed.

...when next run."
  (setq nael--idle-buffer nil))

(defun nael--idle-function ()
  (when (eq major-mode 'nael-mode)
    (let ((old-buffer nael--idle-buffer)
          (old-tick nael--idle-tick))
      (setq nael--idle-buffer (current-buffer))
      (setq nael--idle-tick (buffer-modified-tick))
      ;; If the user has switched buffer or the buffer is not
      ;; modified, refresh the info buffer now. Otherwise (if the
      ;; buffer is modified), do nothing: we will refresh the info
      ;; buffer later, from Eglot's internal
      ;; `eglot--document-changed-hook'.
      (when (or (not (eq nael--idle-buffer old-buffer))
                (eq nael--idle-tick old-tick))
        (nael-info-buffer-refresh)))))

(defun nael--start-idle-timer ()
  (unless nael--idle-timer
    (setq nael--idle-timer
          (run-with-idle-timer nael-idle-delay t
                               #'nael--idle-function))))

(defun nael--cancel-idle-timer ()
  (when nael--idle-timer
    (cancel-timer nael--idle-timer)
    (setq nael--idle-timer nil)))

(nael--start-idle-timer)

;;;###autoload
(define-derived-mode nael-mode prog-mode "Nael"
  "Major mode for Lean.
\\{nael-mode-map}
Invokes `nael-mode-hook'."
  :syntax-table nael-syntax-table
  :group 'nael
  (setq-local comment-start "--")
  (setq-local comment-start-skip "[-/]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding 1)
  (setq-local comment-use-syntax t)
  (setq-local compilation-mode-font-lock-keywords nil)
  (setq-local font-lock-defaults nael-font-lock-defaults)
  (set-input-method "Lean")
  ;; Inhibit flymake from starting automatically. Since diagnostics
  ;; are updated only by the language server, we call `flymake-start'
  ;; on their receipt.
  (setq-local flymake-no-changes-timeout nil)
  (setq-local flymake-start-on-flymake-mode nil)
  (setq-local flymake-start-on-save-buffer nil)
  ;; Let the `next-error' and `previous-error' commands navigate
  ;; diagnostics.
  (setq-local next-error-function #'flymake-goto-next-error))

;;;###autoload
(setf (alist-get "\\.lean\\'" auto-mode-alist nil nil #'equal)
      'nael-mode)

;;;###autoload
(setf (alist-get "lean" markdown-code-lang-modes nil nil #'equal)
      'nael-mode)

(setf (alist-get 'nael-mode eglot-server-programs)
      '(nael-eglot-lsp-server . ("lake" "serve")))

(defclass nael-eglot-lsp-server (eglot-lsp-server) nil
  :documentation "Subclass of `eglot-lsp-server' specifically for Nael")

;; We call `nael-info-buffer-refresh' and `flymake-start' from a
;; timer to reduce nesting of synchronous json requests, with a
;; (short) nonzero delay in case the server sends diagnostics
;; excessively frequently.
(defvar nael--diagnostics-pending nil)
(defun nael--handle-diagnostics (server uri)
  (setq nael--diagnostics-pending nil)
  (let
      ((path
        (abbreviate-file-name
         (file-truename (eglot-uri-to-path uri)))))
    (dolist (buf (eglot--managed-buffers server))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when
              (and buffer-file-name
                   (string= buffer-file-truename path))
            (nael-info-buffer-refresh) (flymake-start)))))))

(cl-defmethod eglot-handle-notification
  :after
  ((server nael-eglot-lsp-server)
   (_ (eql textDocument/publishDiagnostics))
   &key uri
   &allow-other-keys)
  "Handle notification textDocument/publishDiagnostics."
  (unless nael--diagnostics-pending
    (setq nael--diagnostics-pending t)
    (run-with-timer 0.05 nil #'nael--handle-diagnostics server uri)))

(cl-defmethod eglot-register-capability
  ((_server nael-eglot-lsp-server)
   (_method (eql workspace/didChangeWatchedFiles))
   _id &key _watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles."
  (when nael-enable-file-watchers
    (cl-call-next-method)))

(cl-defmethod eglot-unregister-capability
  ((_server nael-eglot-lsp-server)
   (_method (eql workspace/didChangeWatchedFiles))
   _id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles."
  (when nael-enable-file-watchers
    (cl-call-next-method)))

;; Workarounds for code actions, see
;;   https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66552
;;   https://github.com/leanprover/lean4/pull/2721
(defun nael-mode--before--eglot-read-execute-code-action (args)
  "Before advicing `eglot--read-execute-code-action' for Lean server.

For `Try this' quickfixes, append the new text to the title, so the
user knows which item is which."
  (when (eq (type-of (eglot-current-server)) 'nael-eglot-lsp-server)
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
            #'nael-mode--before--eglot-read-execute-code-action)

(cl-defmethod eglot-execute
  :before ((_server nael-eglot-lsp-server) action)
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

(provide 'nael-mode)

;;; nael-mode.el ends here
