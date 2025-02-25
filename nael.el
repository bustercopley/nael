;;; nael.el --- A humble major-mode for Lean  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corp. (R)
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
;; Keywords: languages
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: ((emacs "29.1") (markdown-mode "2"))
;; SPDX-License-Identifier: Apache-2.0 AND GPL-3.0-or-later
;; URL: https://codeberg.org/mekeor/nael
;; Version: 0.2.3

;; This file is not part of GNU Emacs.

;; This program is based on `lean4-mode' which is licensed under
;; Apache-2.0.  Additions and modifications made within this
;; repository are licensed under GNU General Public License version 3
;; or later.

;;; Commentary:

;; Nael is a humble major-mode for Lean.  Its documentation is given
;; as `README.org' which is also provided as Info manual.

;;; Code:

(require 'eglot)
(require 'jsonrpc)
(require 'project)
(require 'rx)
(require 'seq)

(require 'markdown-mode)

(defgroup nael nil
  "A humble major-mode for Lean."
  :group 'languages
  :link '(emacs-library-link
          :tag "Source Lisp File"
          "nael.el")
  :link '(info-link
          :tag "Info manual"
          "(nael)Top")
  :link '(url-link :tag "Website"
                   "https://codeberg.org/mekeor/nael")
  :prefix "nael-"
  :tag "Nael")

;;;; Syntax:

(defvar nael-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Parentheses:
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Comments:
    (modify-syntax-entry ?/  ". 14nb" table)
    (modify-syntax-entry ?-  ". 123"  table)
    (modify-syntax-entry ?\n ">"      table)
    (modify-syntax-entry ?«  "<"      table)
    (modify-syntax-entry ?»  ">"      table)

    ;; Words:
    (mapc
     (lambda (character) (modify-syntax-entry character "w" table))
     '(;; Latin characters:
       ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u
       ?v ?w ?x ?y ?z ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P
       ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ;; Digits
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ;; Greek (and Coptic) characters:
       ?α ?β ?γ ?δ ?ε ?ζ ?η ?θ ?ι ?κ ;; ?λ
       ?μ ?ν ?ξ ?ο ?π ?ρ ?ς ?σ ?τ ?υ ?φ ?χ ?ψ ?ω ?ϊ ?ϋ ?ό ?ύ ?ώ ?Ϗ ?ϐ
       ?ϑ ?ϒ ?ϓ ?ϔ ?ϕ ?ϖ ?ϗ ?Ϙ ?ϙ ?Ϛ ?ϛ ?Ϝ ?ϝ ?Ϟ ?ϟ ?Ϡ ?ϡ ?Ϣ ?ϣ ?Ϥ ?ϥ
       ?Ϧ ?ϧ ?Ϩ ?ϩ ?Ϫ ?ϫ ?Ϭ ?ϭ ?Ϯ ?ϯ ?ϰ ?ϱ ?ϲ ?ϳ ?ϴ ?ϵ ?϶ ?Ϸ ?ϸ ?Ϲ ?Ϻ
       ?ϻ ?ἀ ?ἁ ?ἂ ?ἃ ?ἄ ?ἅ ?ἆ ?ἇ ?Ἀ ?Ἁ ?Ἂ ?Ἃ ?Ἄ ?Ἅ ?Ἆ ?Ἇ ?ἐ ?ἑ ?ἒ ?ἓ
       ?ἔ ?ἕ ?἖ ?἗ ?Ἐ ?Ἑ ?Ἒ ?Ἓ ?Ἔ ?Ἕ ?἞ ?἟ ?ἠ ?ἡ ?ἢ ?ἣ ?ἤ ?ἥ ?ἦ ?ἧ ?Ἠ
       ?Ἡ ?Ἢ ?Ἣ ?Ἤ ?Ἥ ?Ἦ ?Ἧ ?ἰ ?ἱ ?ἲ ?ἳ ?ἴ ?ἵ ?ἶ ?ἷ ?Ἰ ?Ἱ ?Ἲ ?Ἳ ?Ἴ ?Ἵ
       ?Ἶ ?Ἷ ?ὀ ?ὁ ?ὂ ?ὃ ?ὄ ?ὅ ?὆ ?὇ ?Ὀ ?Ὁ ?Ὂ ?Ὃ ?Ὄ ?Ὅ ?὎ ?὏ ?ὐ ?ὑ ?ὒ
       ?ὓ ?ὔ ?ὕ ?ὖ ?ὗ ?὘ ?Ὑ ?὚ ?Ὓ ?὜ ?Ὕ ?὞ ?Ὗ ?ὠ ?ὡ ?ὢ ?ὣ ?ὤ ?ὥ ?ὦ ?ὧ
       ?Ὠ ?Ὡ ?Ὢ ?Ὣ ?Ὤ ?Ὥ ?Ὦ ?Ὧ ?ὰ ?ά ?ὲ ?έ ?ὴ ?ή ?ὶ ?ί ?ὸ ?ό ?ὺ ?ύ ?ὼ
       ?ώ ?὾ ?὿ ?ᾀ ?ᾁ ?ᾂ ?ᾃ ?ᾄ ?ᾅ ?ᾆ ?ᾇ ?ᾈ ?ᾉ ?ᾊ ?ᾋ ?ᾌ ?ᾍ ?ᾎ ?ᾏ ?ᾐ ?ᾑ
       ?ᾒ ?ᾓ ?ᾔ ?ᾕ ?ᾖ ?ᾗ ?ᾘ ?ᾙ ?ᾚ ?ᾛ ?ᾜ ?ᾝ ?ᾞ ?ᾟ ?ᾠ ?ᾡ ?ᾢ ?ᾣ ?ᾤ ?ᾥ ?ᾦ
       ?ᾧ ?ᾨ ?ᾩ ?ᾪ ?ᾫ ?ᾬ ?ᾭ ?ᾮ ?ᾯ ?ᾰ ?ᾱ ?ᾲ ?ᾳ ?ᾴ ?᾵ ?ᾶ ?ᾷ ?Ᾰ ?Ᾱ ?Ὰ ?Ά
       ?ᾼ ?᾽ ?ι ?᾿ ?῀ ?῁ ?ῂ ?ῃ ?ῄ ?῅ ?ῆ ?ῇ ?Ὲ ?Έ ?Ὴ ?Ή ?ῌ ?῍ ?῎ ?῏ ?ῐ
       ?ῑ ?ῒ ?ΐ ?῔ ?῕ ?ῖ ?ῗ ?Ῐ ?Ῑ ?Ὶ ?Ί ?῜ ?῝ ?῞ ?῟ ?ῠ ?ῡ ?ῢ ?ΰ ?ῤ ?ῥ
       ?ῦ ?ῧ ?Ῠ ?Ῡ ?Ὺ ?Ύ ?Ῥ ?῭ ?΅ ?` ?῰ ?῱ ?ῲ ?ῳ ?ῴ ?῵ ?ῶ ?ῷ ?Ὸ ?Ό ?Ὼ
       ?Ώ ?ῼ ?´ ?῾
       ;; Mathematical characters:
       ?℀ ?℁ ?ℂ ?℃ ?℄ ?℅ ?℆ ?ℇ ?℈ ?℉ ?ℊ ?ℋ ?ℌ ?ℍ ?ℎ ?ℏ ?ℐ ?ℑ ?ℒ ?ℓ ?℔
       ?ℕ ?№ ?℗ ?℘ ?ℙ ?ℚ ?ℛ ?ℜ ?ℝ ?℞ ?℟ ?℠ ?℡ ?™ ?℣ ?ℤ ?℥ ?Ω ?℧ ?ℨ ?℩
       ?K ?Å ?ℬ ?ℭ ?℮ ?ℯ ?ℰ ?ℱ ?Ⅎ ?ℳ ?ℴ ?ℵ ?ℶ ?ℷ ?ℸ ?ℹ ?℺ ?℻ ?ℼ ?ℽ ?ℾ
       ?ℿ ?⅀ ?⅁ ?⅂ ?⅃ ?⅄ ?ⅅ ?ⅆ ?ⅇ ?ⅈ ?ⅉ ?⅊ ?⅋ ?⅌ ?⅍ ?ⅎ ?⅏
       ;; Subscripts:
       ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀ ?ₐ ?ₑ ?ₒ ?ₓ ?ₔ ?ₕ ?ₖ ?ₗ ?ₘ ?ₙ ?ₚ
       ?ₛ ?ₜ ?' ?_ ?! ??))

    ;; Operators:
    (mapc
     (lambda (character) (modify-syntax-entry character "." table))
     '(?# ?$ ?% ?& ?* ?+ ?< ?= ?> ?@ ?^ ?| ?~ ?:))

    ;; Whitespace:
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)

    ;; Strings:
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "/"  table)

    table)
  "Syntax table used in Nael mode.")

(defvar nael-font-lock-defaults
  (list
   (list
    (list (rx word-start "attribute" word-end
              (zero-or-more space)
              (group
               (one-or-more "[" (zero-or-more (not (any "]"))) "]"
                            (zero-or-more space))))
          '(1 'font-lock-preprocessor-face))
    (list (rx (group "@[" (zero-or-more (not (any "]"))) "]"))
          '(1 'font-lock-preprocessor-face))
    (list (rx (group "#"
                     (or "eval" "print" "reduce" "help" "check" "lang"
                         "check_failure" "synth")))
          '(1 'font-lock-keyword-face))

    ;; Mutual definitions:
    (list (rx word-start "mutual" word-end
              (zero-or-more space)
              word-start (or "inductive" "definition" "def") word-end
              (group (zero-or-more (not (any " \t\n\r{([,")))
                     (zero-or-more (zero-or-more space) ","
                                   (zero-or-more space)
                                   (not (any " \t\n\r{([,")))))
          '(1 'font-lock-function-name-face))

    ;; Declarations:
    (list (rx word-start
              (group
               (or "axiom" "class" "constant" "def" "definition"
                   "inductive" "instance" "lemma" "opaque"
                   "structure" "theorem"
                   (group "class"
                          (zero-or-more space)
                          "inductive")))
              word-end
              (zero-or-more space)
              (group (zero-or-more
                      "{" (zero-or-more (not (any "}"))) "}"
                      (zero-or-more space)))
              (zero-or-more space)
              (group (zero-or-more (not (any " \t\n\r{([")))))
          '(4 'font-lock-function-name-face))

    ;; Constants which have a keyword as subterm:
    (cons "∘if"
          'font-lock-constant-face)

    ;; Keywords:
    (list "\\(set_option\\)[ \t]*\\([^ \t\n]*\\)"
          '(2 'font-lock-constant-face))
    (cons (rx word-start
              (or
               "abbrev" "at" "attribute" "attributes" "axiom" "begin"
               "break" "builtin_initialize" "by" "cases" "catch"
               "class" "constant" "continue" "declare_syntax_cat"
               "def" "deriving" "do" "elab" "else" "end" "example"
               "exists" "export" "extends" "finally" "for" "forall"
               "from" "fun" "generalizing" "have" "hide" "hiding"
               "if" "import" "in" "include" "induction" "inductive"
               "infix" "infixl" "infixr" "init_quot" "initialize"
               "instance" "lemma" "let" "local" "macro" "macro_rules"
               "match" "match_syntax" "mut" "mutual" "namespace"
               "nomatch" "noncomputable" "notation" "open" "opaque"
               "partial" "postfix" "precedence" "prefix" "prelude"
               "private" "protected" "raw" "rec"
               "register_builtin_option" "renaming" "return" "run_cmd"
               "scoped" "section" "set_option" "show" "structure"
               "suffices" "syntax" "then" "theorem" "this" "try"
               "unif_hint" "universe" "universes" "unless" "unsafe"
               "using" "using_well_founded" "variable" "variables"
               "where" "with")
              word-end)
          'font-lock-keyword-face)
    (list (rx word-start (group "example") ".")
          '(1 'font-lock-keyword-face))
    (cons (rx (or "∎"))
          'font-lock-keyword-face)

    ;; Types:
    (cons (rx word-start
              (or "Prop" "Type" "Type*" "Sort" "Sort*")
              symbol-end)
          'font-lock-type-face)
    (list (rx word-start (group (or "Prop" "Type" "Sort")) ".")
          '(1 'font-lock-type-face))

    ;; Strings:
    (cons "\"[^\"]*\"" 'font-lock-string-face)

    ;; Constants:
    (cons (regexp-opt
           '("!" "#" "$" "&&" "*" "+" "+c" "+f" "+n" "-" "->" "/" "/"
             "/\\" ":=" "<" "<->" "<=" "=" "==" ">" ">=" "@" "\\/"
             "^c" "||" "~" "¬" "×" "×c" "×f" "×n" "Π" "Σ" "λ" "⁻¹" "ℂ"
             "ℕ" "ℕ₋₂" "ℚ" "ℝ" "ℤ" "→" "↔" "∀" "∃" "∘" "∘1nf" "∘f"
             "∘f1n" "∘fi" "∘fn" "∘fn1" "∘n" "∘n1f" "∘nf" "∧" "∨" "∼"
             "≃" "≃c" "≅" "≅c" "≠" "≡" "≤" "≥" "▸" "◾" "◾o" "⬝" "⬝e"
             "⬝h" "⬝hp" "⬝i" "⬝o" "⬝op" "⬝ph" "⬝po" "⬝pv" "⬝r" "⬝v"
             "⬝vp" "𝔸"))
          'font-lock-constant-face)
    (cons (rx word-start
              (one-or-more digit)
              (optional (and "." (zero-or-more digit)))
              word-end)
          'font-lock-constant-face)

    ;; Place holder:
    (cons (rx symbol-start "_" symbol-end)
          'font-lock-preprocessor-face)

    ;; Warnings:
    (cons (rx word-start "sorry" word-end)
          'font-lock-warning-face)
    (cons (rx word-start
              (or "assert" "dbgTrace" "panic" "unreachable"))
          'font-lock-warning-face)

    ;; Escaped identifiers:
    (list (rx (group "«")
              (group (one-or-more (not (any "»"))))
              (group "»"))
          '(1 font-lock-comment-face t)
          '(2 nil t)
          '(3 font-lock-comment-face t))))
  "Defaults for Font Lock mode used by Nael mode.")

(defvar nael-imenu-generic-expression
  (list (list nil
              ;; The following `rx'-expression is duplicated from
              ;; definition of `nael-font-lock-defaults'.
              (rx line-start ;; Use `line-start' rather than
                             ;; `word-start' for speed.
                  (group
                   (or "axiom" "class" "constant" "def" "definition"
                       "inductive" "instance" "lemma" "opaque"
                       "structure" "theorem"
                       (group "class"
                              (zero-or-more space)
                              "inductive")))
                  word-end
                  (zero-or-more space)
                  (group (zero-or-more
                          "{" (zero-or-more (not (any "}"))) "}"
                          (zero-or-more space)))
                  (zero-or-more space)
                  (group (zero-or-more (not (any " \t\n\r{([")))))
              4))
  "`imenu-generic-expression' for Nael mode.

This is only in effect when Eglot is not enabled.")

;;;; Eglot (language server):

;; Use "lake serve" as language-server.
(setf (alist-get 'nael-mode eglot-server-programs)
      '("lake" "serve"))

(defface nael-eglot-eldoc-header
  '((t (:extend t :inherit markdown-header-face-1)))
  "Face for section-headers of Nael-specific ElDoc documentations."
  :group 'nael)

(defvar nael-eglot-eldoc-fontify-buffer-name
  "*Nael Eglot ElDoc Fontify*"
  "Name of buffer that is reused in order to fontify Nael code.")

(defun nael-eglot-eldoc-fontify (string)
  "Apply Nael font-lock rules to STRING."
  (with-current-buffer
      (get-buffer-create nael-eglot-eldoc-fontify-buffer-name)
    (erase-buffer)
    (insert string)
    (setq-local font-lock-defaults nael-font-lock-defaults)
    (font-lock-ensure)
    (buffer-string)))

(defun nael-eglot-eldoc-plain-goal (cb)
  "`PlainGoal' for `eldoc-documentation-functions'.

CB is the callback provided to members of ElDoc documentation
functions.

https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/\
Extra.html#Lean.Lsp.PlainGoal"
  (jsonrpc-async-request
   (eglot--current-server-or-lose)
   :$/lean/plainGoal
   (eglot--TextDocumentPositionParams)
   :success-fn
   (lambda (response)
     (apply
      cb
      (if-let*
          ((goals (plist-get response :goals))
           ;; Since `goals' is not a list, we separately need to
           ;; ensure it's not empty.
           ((not (seq-empty-p goals)))
           (first-goal (seq-first goals))
           (first-goal (nael-eglot-eldoc-fontify first-goal)))
          (list (concat
                 ;; Propertize the first newline so that a potential
                 ;; t-valued `:extend' face-attribute works correctly.
                 (propertize "Tactic state:\n"
                             'face 'nael-eglot-eldoc-header)
                 "\n"
                 ;; Re-use the previously rendered documentation of
                 ;; the first goal rather than rendering it again.
                 (replace-regexp-in-string "^" "  " first-goal)
                 (seq-mapcat
                  (lambda (goal)
                    (concat "\n\n" (replace-regexp-in-string
                                    "^" "  "
                                    (nael-eglot-eldoc-fontify goal))))
                  (seq-drop goals 1) 'string)
                 "\n")
                :echo first-goal)
        (list nil)))))
  t)

(defun nael-eglot-eldoc-plain-term-goal (cb)
  "`PlainTermGoal' for `eldoc-documentation-functions'.

CB is the callback provided to members of ElDoc documentation
functions.

https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/\
Extra.html#Lean.Lsp.PlainTermGoal"
  (jsonrpc-async-request
   (eglot--current-server-or-lose)
   :$/lean/plainTermGoal
   (eglot--TextDocumentPositionParams)
   :success-fn
   (lambda (response)
     (apply
      cb
      (if-let*
          ((goal (plist-get response :goal))
           ((not (string= "" goal)))
           (doc (eglot--format-markup goal)))
          (list (concat
                 ;; Propertize the first newline so that a potential
                 ;; t-valued `:extend' face-attribute works correctly.
                 (propertize "Expected type:\n"
                             'face 'nael-eglot-eldoc-header)
                 "\n"
                 (replace-regexp-in-string "^" "  " doc)
                 "\n")
                ;; Don't echo any docstring at all.
                :echo 'skip)
        (list nil)))))
  t)

(defun nael-eglot-managed-setup ()
  "Buffer-locally setup ElDoc for Nael.

Use ElDoc documentation strategy `compose' and add ElDoc documentation
functions for `plain-goal' and `plain-term-goal'."
  (interactive)
  (setq-local eldoc-documentation-strategy
              #'eldoc-documentation-compose)
  (add-hook 'eldoc-documentation-functions
            #'nael-eglot-eldoc-plain-goal -90 'local)
  (add-hook 'eldoc-documentation-functions
            #'nael-eglot-eldoc-plain-term-goal -80 'local))

;;;; Mode:

(defcustom nael-mode-hook nil
  "Hook run when entering Nael mode."
  :options '(eglot-ensure imenu-add-menubar-index)
  :type 'hook
  :group 'nael)

(define-derived-mode nael-mode prog-mode "Nael"
  "A humble major-mode for Lean.

\\{nael-mode-map}"
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
  (setq-local parse-sexp-ignore-comments
              t)
  ;; Font-lock:
  (setq-local font-lock-defaults
              nael-font-lock-defaults)
  ;; Compile:
  (setq-local compilation-mode-font-lock-keywords
              nil)
  (setq-local compile-command
              "lake build ")
  ;; Imenu:
  (setq-local imenu-generic-expression
              nael-imenu-generic-expression)
  ;; Flymake:
  (setq-local next-error-function
              #'flymake-goto-next-error)
  ;; Eglot: Don't wait for "lake serve" to output anything when
  ;; launched because it won't. (bug#1)
  (setq-local eglot-sync-connect
              nil)
  (add-hook 'eglot-managed-mode-hook
            #'nael-eglot-managed-setup nil 'local))

;;;; Project:

;; Use Lakefiles as Project root-markers.  In particular, this will
;; make "lake serve" (the language-server associated with Nael) and
;; "lake build" (the `compile-command' in Nael) work.
(add-to-list 'project-vc-extra-root-markers
             "lakefile.lean")

;;;; File extension:

;; Associate the ".lean" file-extension with Nael.
(setf (alist-get "\\.lean\\'" auto-mode-alist nil nil #'equal)
      'nael-mode)

;; Associate the "lean" language-specification in markdown code-fences
;; with Nael.
(setf (alist-get "lean" markdown-code-lang-modes nil nil #'equal)
      'nael-mode)

;;;; End:

(provide 'nael)

;;; nael.el ends here
