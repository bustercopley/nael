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

(require 'lean4-dev)
(require 'lean4-eri)
(require 'lean4-fringe)
(require 'lean4-info)
(require 'lean4-lake)
(require 'lean4-settings)
(require 'lean4-syntax)
(require 'lean4-util)

;; TODO: Get rid of this by never calling `lean-mode'.
(declare-function lean-mode "ext:lean-mode")

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

(defun lean4-execute (&optional arg)
  "Execute Lean in the current buffer with an optional argument ARG."
  (interactive)
  (when (called-interactively-p 'any)
    (setq arg (read-string "arg: " arg)))
  (let*
      ((cc compile-command)
	   (dd default-directory)
	   (use-lake (lean4-lake-find-dir))
	   (default-directory (if use-lake (lean4-lake-find-dir) dd))
       (target-file-name
        (or
         (buffer-file-name)
         (flymake-proc-init-create-temp-buffer-copy
          'lean4-create-temp-in-system-tempdir))))
    (compile
     (lean4-compile-string
	  (if use-lake
          (shell-quote-argument
           (expand-file-name (lean4-get-executable lean4-lake-name)))
        nil)
      (shell-quote-argument
       (expand-file-name
        (lean4-get-executable lean4-executable-name)))
      (or arg "")
      (shell-quote-argument (expand-file-name target-file-name))))
    ;; restore old value
    (setq compile-command cc)
    (setq default-directory dd)))

(defun lean4-std-exe ()
  "Execute Lean in the current buffer."
  (interactive)
  (lean4-execute))

(defun lean4-refresh-file-dependencies ()
  "Refresh the file dependencies.

This function restarts the server subprocess for the current
file, recompiling, and reloading all imports."
  (interactive)
  (when eglot--managed-mode
    (eglot--signal-textDocument/didClose)
    (eglot--signal-textDocument/didOpen)))

(defun lean4-indent-line ()
  "Lean 4 indent line function.

If point is at the end of the current indentation, use
`lean4-eri-indent'; or if point is before that position, move it
there; or do nothing, to allow tab completion (if configured)."
  (let ((cur-column (current-column))
        (cur-indent (current-indentation)))
    (cond ((= cur-column cur-indent)
           (lean4-eri-indent))
          ((< cur-column cur-indent)
           (move-to-column cur-indent)))))

;; TODO: Get rid of this function (and all the settings variables) by
;; simply defining these keybindings in the mode map.
(defun lean4-set-keys ()
  "Setup Lean 4 keybindings."
  (local-set-key lean4-keybinding-std-exe1
                 #'lean4-std-exe)
  (local-set-key lean4-keybinding-std-exe2
                 #'lean4-std-exe)
  (local-set-key lean4-keybinding-show-key
                 #'quail-show-key)
  ;; (local-set-key lean4-keybinding-hole
  ;;                #'lean4-hole)
  (local-set-key lean4-keybinding-lean4-toggle-info
                 #'lean4-toggle-info)
  ;; (local-set-key lean4-keybinding-lean4-message-boxes-toggle
  ;;                #'lean4-message-boxes-toggle)
  (local-set-key lean4-keybinding-lake-build
                 #'lean4-lake-build)
  (local-set-key lean4-keybinding-refresh-file-dependencies
                 #'lean4-refresh-file-dependencies)
  ;; This only works as a mouse binding due to the event, so it is not
  ;; abstracted to avoid user confusion.
  ;; (local-set-key (kbd "<mouse-3>")
  ;;                #'lean4-right-click-show-menu)
  )

(define-abbrev-table 'lean4-abbrev-table nil)

(defvar lean4-mode-map (make-sparse-keymap)
  "Keymap used in Lean mode.")

(easy-menu-define lean4-mode-menu lean4-mode-map
  "Menu for the Lean major mode."
  '("Lean 4"
    ["Execute lean"         lean4-execute           t]
    ["Toggle info display"  lean4-toggle-info       t]
    ["Restart lean process" eglot-reconnect         t]
    ["Customize lean4-mode" (customize-group 'lean) t]))

(defvar lean4-idle-hook nil
  "Hook run after Emacs has been idle for `lean4-idle-delay' seconds.

The functions are run only once for each time Emacs becomes idle.")

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

(cl-defmethod project-root ((project (head lake)))
  "Pair ('lake . DIR) is Lean project whose root directory is DIR.

This will allow us to use Emacs when a repo contains multiple lean
packages."
  (cdr project))

(defcustom lean4-workspace-exclusions nil
  "Set of directories in which not to start a Lean 4 language server."
  :group 'lean4
  :type '(repeat directory))

(defcustom lean4-workspace-roots nil
  "A set of directories in which to start a Lean 4 language server."
  :group 'lean4
  :type '(repeat directory))

(defvar lean4--workspace-message-enabled nil)

(defun lean4-project-find (file-name)
  "Find the workspace root directory for a Lean 4 file.

Files under the same root directory use the same instance of
the Lean 4 language server.

Look up the directory hierarchy starting from FILE-NAME for the
first member of `lean4-workspace-roots' or
`lean4-workspace-exclusions'. If no such directory is found,
search again and use the *last* directory containing a file
\"lean-toolchain\". If the second search fails, or if the search
encounters a member of `lean4-workspace-exclusions', do not start
a language server instance."
  (when (or (bound-and-true-p eglot-lsp-context)
            lean4--workspace-message-enabled)
    (let*
        ((normalize
          (lambda (dir) (abbreviate-file-name (file-truename dir))))
         (roots (mapcar normalize lean4-workspace-roots))
         (excls (mapcar normalize lean4-workspace-exclusions))
         (ignore-case (file-name-case-insensitive-p file-name))
         (contains
          (lambda (file-name list)
            (seq-some
             (lambda (f)
               (if ignore-case
                   (string-equal-ignore-case file-name f)
                 (string-equal file-name f)))
             list)))
         root
         excluded)
      ;; Search for configured roots and exclusions.
      (if-let
          ((dir
            (locate-dominating-file
             file-name
             (lambda (file-name)
               (when (file-directory-p file-name)
                 (or (funcall contains file-name roots)
                     (setq excluded
                           (funcall contains file-name excls))))))))
          (unless excluded (setq root dir))
        ;; Configured directory not found.  Now search for a toolchain
        ;; file.
        (while-let
            ((dir
              (locate-dominating-file file-name "lean-toolchain")))
          ;; We found a toolchain file, but maybe it belongs to a
          ;; package.  Continue looking until there are no more
          ;; toolchain files.
          (setq root dir)
          (setq file-name
                (file-name-directory (directory-file-name dir)))))
      (if root
          (cons 'lake root)
        (when (and lean4--workspace-message-enabled (not excluded))
          (message
           (concat
            "File does not belong to a workspace and no lakefile "
            "found.  Customize the variables `lean4-workspace-roots' "
            "and `lean4-workspace-exclusions' to define "
            "workspaces."))
          nil)))))

(push #'lean4-project-find project-find-functions)

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
  (setq-local indent-line-function 'lean4-indent-line)
  ;; Inhibit flymake from starting automatically. Since diagnostics
  ;; are updated only by the language server, we call `flymake-start'
  ;; on their receipt.
  (setq-local flymake-no-changes-timeout nil)
  (setq-local flymake-start-on-flymake-mode nil)
  (setq-local flymake-start-on-save-buffer nil)
  ;; Let the `next-error' and `previous-error' commands navigate
  ;; diagnostics.
  (setq-local next-error-function 'flymake-goto-next-error)
  (lean4-set-keys)
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1))
  ;; (abbrev-mode 1)
  (when buffer-file-name
    (let ((lean4--workspace-message-enabled t))
      (if (lean4-project-find buffer-file-truename)
          (progn
            (eglot-ensure)
            (add-hook 'before-save-hook
                      #'lean4-whitespace-cleanup nil 'local))))))

(defun lean4--version ()
  "Return Lean version as a list `(MAJOR MINOR PATCH)'."
  (with-temp-buffer
    (call-process (lean4-get-executable "lean")
                  nil (list t nil) nil "-v")
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
(defun lean4-select-mode ()
  "Automatically select mode (Lean 3 vs Lean 4)."
  (if (and lean4-autodetect-lean3
           (eq 3 (car (lean4--version))))
      (lean-mode)
    (lean4-mode)))

;; Automatically use lean4-mode for .lean files.
;;;###autoload
(push '("\\.lean\\'" . lean4-select-mode) auto-mode-alist)

;;;###autoload
(add-to-list 'markdown-code-lang-modes '("lean" . lean4-select-mode))

;; Use utf-8 encoding
;;;###autoload
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

(defun lean4--server-cmd ()
  "Return Lean server command.

If found lake version at least 3.1.0, then return '/path/to/lake
serve', otherwise return '/path/to/lean --server'."
  (condition-case nil
      (if (string-version-lessp
           (car
            (process-lines (lean4-get-executable "lake") "--version"))
           "3.1.0")
          `(,(lean4-get-executable lean4-executable-name) "--server")
        `(,(lean4-get-executable "lake") "serve"))
    (error
     `(,(lean4-get-executable lean4-executable-name) "--server"))))

;; Eglot init
(defun lean4--server-class-init (&optional _interactive)
  (cons 'lean4-eglot-lsp-server (lean4--server-cmd)))

(push (cons 'lean4-mode #'lean4--server-class-init)
      eglot-server-programs)

(defclass lean4-eglot-lsp-server (eglot-lsp-server) nil
  :documentation "Eglot LSP server subclass for the Lean 4 server.")

(cl-defmethod eglot-handle-notification
  ((server lean4-eglot-lsp-server)
   (_method (eql $/lean/fileProgress))
   &key textDocument processing)
  "Handle notification $/lean/fileProgress."
  (eglot--dbind ((VersionedTextDocumentIdentifier) uri) textDocument
    (lean4-fringe-update server processing uri)))

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
  :after ((server lean4-eglot-lsp-server)
          (_method (eql textDocument/publishDiagnostics))
          &key uri &allow-other-keys)
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

For \"Try this\" quickfixes, append the new text to the title, so the
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
