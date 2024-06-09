;;; nael.el --- Major mode for Lean -*- lexical-binding: t; -*-

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

(defgroup nael nil
  "Lean programming language and theorem prover."
  :prefix "nael-"
  :group 'languages)

(defcustom nael-enable-file-watchers nil
  "Honour requests from the server to watch for file modifications.

This is disabled by default because the server wants to watch
`**/*.ilean', and in many cases there are too many directories to
watch each individually."
  :group 'nael :type 'boolean)

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
          (lambda (file-name prefix)
            (make-temp-file (or prefix "flymake") nil
                            (file-name-extension file-name)))))))
    (compile
     (format
      (concat (if use-lake "lake env lean" "lean") "%s %s")
      (or arg "")
      (shell-quote-argument (expand-file-name target-file-name))))))

(defvar nael-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Matching parens
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; comment
    (modify-syntax-entry ?/ ". 14nb" table)
    (modify-syntax-entry ?- ". 123" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?« "<" table)
    (modify-syntax-entry ?» ">" table)

    ;; Word constituent
    (dolist
        (chr
         '( ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
            ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
            ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
            ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z))
      (modify-syntax-entry chr "w" table))
    (dolist
        (chr
         '( ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
      (modify-syntax-entry chr "w" table))
    (dolist
        (chr
         '( ?α ?β ?γ ?δ ?ε ?ζ ?η ?θ ?ι ?κ ;;?λ
            ?μ ?ν ?ξ ?ο ?π ?ρ ?ς ?σ ?τ ?υ
            ?φ ?χ ?ψ ?ω))
      (modify-syntax-entry chr "w" table))
    (dolist
        (chr
         '( ?ϊ ?ϋ ?ό ?ύ ?ώ ?Ϗ ?ϐ ?ϑ ?ϒ ?ϓ ?ϔ ?ϕ ?ϖ
            ?ϗ ?Ϙ ?ϙ ?Ϛ ?ϛ ?Ϝ ?ϝ ?Ϟ ?ϟ ?Ϡ ?ϡ ?Ϣ ?ϣ
            ?Ϥ ?ϥ ?Ϧ ?ϧ ?Ϩ ?ϩ ?Ϫ ?ϫ ?Ϭ ?ϭ ?Ϯ ?ϯ ?ϰ
            ?ϱ ?ϲ ?ϳ ?ϴ ?ϵ ?϶ ?Ϸ ?ϸ ?Ϲ ?Ϻ ?ϻ))
      (modify-syntax-entry chr "w" table))
    (dolist
        (chr
         '( ?ἀ ?ἁ ?ἂ ?ἃ ?ἄ ?ἅ ?ἆ ?ἇ ?Ἀ ?Ἁ ?Ἂ ?Ἃ ?Ἄ
            ?Ἅ ?Ἆ ?Ἇ ?ἐ ?ἑ ?ἒ ?ἓ ?ἔ ?ἕ ?἖ ?἗ ?Ἐ ?Ἑ
            ?Ἒ ?Ἓ ?Ἔ ?Ἕ ?἞ ?἟ ?ἠ ?ἡ ?ἢ ?ἣ ?ἤ ?ἥ
            ?ἦ ?ἧ ?Ἠ ?Ἡ ?Ἢ ?Ἣ ?Ἤ ?Ἥ ?Ἦ ?Ἧ ?ἰ ?ἱ
            ?ἲ ?ἳ ?ἴ ?ἵ ?ἶ ?ἷ ?Ἰ ?Ἱ ?Ἲ ?Ἳ ?Ἴ ?Ἵ ?Ἶ ?Ἷ
            ?ὀ ?ὁ ?ὂ ?ὃ ?ὄ ?ὅ ?὆ ?὇ ?Ὀ ?Ὁ ?Ὂ ?Ὃ
            ?Ὄ ?Ὅ ?὎ ?὏ ?ὐ ?ὑ ?ὒ ?ὓ ?ὔ ?ὕ ?ὖ ?ὗ
            ?὘ ?Ὑ ?὚ ?Ὓ ?὜ ?Ὕ ?὞ ?Ὗ ?ὠ ?ὡ ?ὢ
            ?ὣ ?ὤ ?ὥ ?ὦ ?ὧ ?Ὠ ?Ὡ ?Ὢ ?Ὣ ?Ὤ ?Ὥ ?Ὦ
            ?Ὧ ?ὰ ?ά ?ὲ ?έ ?ὴ ?ή ?ὶ ?ί ?ὸ ?ό ?ὺ ?ύ ?ὼ
            ?ώ ?὾ ?὿ ?ᾀ ?ᾁ ?ᾂ ?ᾃ ?ᾄ ?ᾅ ?ᾆ ?ᾇ ?ᾈ
            ?ᾉ ?ᾊ ?ᾋ ?ᾌ ?ᾍ ?ᾎ ?ᾏ ?ᾐ ?ᾑ ?ᾒ ?ᾓ ?ᾔ
            ?ᾕ ?ᾖ ?ᾗ ?ᾘ ?ᾙ ?ᾚ ?ᾛ ?ᾜ ?ᾝ ?ᾞ ?ᾟ ?ᾠ ?ᾡ ?ᾢ
            ?ᾣ ?ᾤ ?ᾥ ?ᾦ ?ᾧ ?ᾨ ?ᾩ ?ᾪ ?ᾫ ?ᾬ ?ᾭ ?ᾮ ?ᾯ ?ᾰ
            ?ᾱ ?ᾲ ?ᾳ ?ᾴ ?᾵ ?ᾶ ?ᾷ ?Ᾰ ?Ᾱ ?Ὰ ?Ά ?ᾼ ?᾽
            ?ι ?᾿ ?῀ ?῁ ?ῂ ?ῃ ?ῄ ?῅ ?ῆ ?ῇ ?Ὲ ?Έ ?Ὴ
            ?Ή ?ῌ ?῍ ?῎ ?῏ ?ῐ ?ῑ ?ῒ ?ΐ ?῔ ?῕ ?ῖ ?ῗ
            ?Ῐ ?Ῑ ?Ὶ ?Ί ?῜ ?῝ ?῞ ?῟ ?ῠ ?ῡ ?ῢ ?ΰ ?ῤ ?ῥ
            ?ῦ ?ῧ ?Ῠ ?Ῡ ?Ὺ ?Ύ ?Ῥ ?῭ ?΅ ?` ?῰ ?῱ ?ῲ ?ῳ
            ?ῴ ?῵ ?ῶ ?ῷ ?Ὸ ?Ό ?Ὼ ?Ώ ?ῼ ?´ ?῾))
      (modify-syntax-entry chr "w" table))
    (dolist
        (chr
         '( ?℀ ?℁ ?ℂ ?℃ ?℄ ?℅ ?℆ ?ℇ ?℈ ?℉ ?ℊ ?ℋ ?ℌ ?ℍ ?ℎ
            ?ℏ ?ℐ ?ℑ ?ℒ ?ℓ ?℔ ?ℕ ?№ ?℗ ?℘ ?ℙ ?ℚ ?ℛ ?ℜ ?ℝ
            ?℞ ?℟ ?℠ ?℡ ?™ ?℣ ?ℤ ?℥ ?Ω ?℧ ?ℨ ?℩ ?K ?Å ?ℬ
            ?ℭ ?℮ ?ℯ ?ℰ ?ℱ ?Ⅎ ?ℳ ?ℴ ?ℵ ?ℶ ?ℷ ?ℸ ?ℹ ?℺ ?℻
            ?ℼ ?ℽ ?ℾ ?ℿ ?⅀ ?⅁ ?⅂ ?⅃ ?⅄ ?ⅅ ?ⅆ ?ⅇ ?ⅈ ?ⅉ ?⅊
            ?⅋ ?⅌ ?⅍ ?ⅎ ?⅏))
      (modify-syntax-entry chr "w" table))
    (dolist
        (chr
         '( ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀
            ?ₐ ?ₑ ?ₒ ?ₓ ?ₔ ?ₕ ?ₖ ?ₗ ?ₘ ?ₙ ?ₚ ?ₛ ?ₜ
            ?' ?_ ?! ??))
      (modify-syntax-entry chr "w" table))

    ;; Lean operator chars
    (dolist
        (chr (string-to-list "#$%&*+<=>@^|~:"))
      (modify-syntax-entry chr "." table))

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "/" table)

    table))

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

;;;###autoload
(define-derived-mode nael-mode prog-mode "Nael"
  "Major mode for editing Lean files.

\\{nael-mode-map}"

  ;; Input:
  (set-input-method "Lean")

  ;; Comments:
  (setq-local comment-end
              "")
  (setq-local comment-end-skip
              "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding
              1)
  (setq-local comment-start
              "--")
  (setq-local comment-start-skip
              "[-/]-[ \t]*")
  (setq-local comment-use-syntax
              t)

  ;; Fontification:
  (setq-local compilation-mode-font-lock-keywords
              nil)
  (setq-local font-lock-defaults
              nael-font-lock-defaults)

  ;; Flymake: Inhibit flymake from starting automatically. Since
  ;; diagnostics are updated only by the language server, we call
  ;; `flymake-start' on their receipt.
  (setq-local flymake-no-changes-timeout nil)
  (setq-local flymake-start-on-flymake-mode nil)
  (setq-local flymake-start-on-save-buffer nil)
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
