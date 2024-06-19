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
;; Homepage: https://codeberg.org/mekeor/emacs-lean
;; Keywords: languages
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: ((emacs "29.1") (markdown-mode "2"))
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later
;; Version: 0.2

;; This file is not part of GNU Emacs.

;; This program is based on `lean4-mode' which is licensed under
;; Apache-2.0.  Additions and modifications made within this
;; repository are licensed under GNU General Public License version 3
;; or later.

;;; Commentary:

;; A humble major-mode for Lean.  Its documentation is given as
;; `README.org' which is also provided as Info manual.

;;; Code:

(require 'jsonrpc)
(require 'eglot)
(require 'project)
(require 'rx)

(require 'markdown-mode)

(defgroup nael nil
  "A humble major-mode for Lean."
  :prefix "nael-"
  :group 'languages)

;;;; Syntax table:

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

    table))

;;;; Font lock:

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
               (or "inductive"
                   (group "class"
                          (zero-or-more space)
                          "inductive")
                   "instance" "structure" "class" "theorem"
                   "axiom" "lemma" "definition" "def" "constant"))
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
               "nomatch" "noncomputable" "notation" "open" "partial"
               "postfix" "precedence" "prefix" "prelude" "private"
               "protected" "raw" "rec" "register_builtin_option"
               "renaming" "return" "run_cmd" "scoped" "section"
               "set_option" "show" "structure" "suffices" "syntax"
               "then" "theorem" "this" "try" "unif_hint" "universe"
               "universes" "unless" "unsafe" "using"
               "using_well_founded" "variable" "variables" "where"
               "with")
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
            "/\\" ":=" "<" "<->" "<=" "=" "==" ">" ">=" "@" "\\/" "^c"
            "||" "~" "¬" "×" "×c" "×f" "×n" "Π" "Σ" "λ" "⁻¹" "ℂ" "ℕ"
            "ℕ₋₂" "ℚ" "ℝ" "ℤ" "→" "↔" "∀" "∃" "∘" "∘1nf" "∘f" "∘f1n"
            "∘fi" "∘fn" "∘fn1" "∘n" "∘n1f" "∘nf" "∧" "∨" "∼" "≃" "≃c"
            "≅" "≅c" "≠" "≡" "≤" "≥" "▸" "◾" "◾o" "⬝" "⬝e" "⬝h" "⬝hp"
            "⬝i" "⬝o" "⬝op" "⬝ph" "⬝po" "⬝pv" "⬝r" "⬝v" "⬝vp" "𝔸"))
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
         '(3 font-lock-comment-face t)))))

;;;; Eglot (language server):

(setf (alist-get 'nael-mode eglot-server-programs)
      '("lake" "serve"))

(defface nael-eldoc-title
  '((t (:extend t :inherit outline-1)))
  "Title of sections `Goal' and `Term Goal' in ElDoc buffer."
  :group 'nael)

(defun nael-eglot-plain-goal-eldoc-function (cb)
  "`PlainGoal' for `eldoc-documentation-functions'.

CB is the callback provided to members of ElDoc documentation
functions.

https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/Extra.html#Lean.Lsp.PlainGoal"
  (jsonrpc-async-request
   (eglot--current-server-or-lose)
   :$/lean/plainGoal
   (eglot--TextDocumentPositionParams)
   :success-fn
   (lambda (response)
     (apply
      cb
      (if-let*
          ((rendered (plist-get response :rendered))
           ((not (string= "" rendered)))
           ((not (string= "no goals" rendered)))
           (doc (eglot--format-markup rendered)))
          (list
           (concat
            ;; Use wording from Lean documentation:
            ;; https://github.com/leanprover/lean4/blob/6fce8f7d5cd18a4419bca7fd51780c71c9b1cc5a/src/Lean/Data/Lsp/Extra.lean#L84
            (propertize "Current goals:\n" 'face 'nael-eldoc-title)
            doc)
           :echo doc)
        (list nil)))))
  t)

(defun nael-eglot-plain-term-goal-eldoc-function (cb)
  "`PlainTermGoal' for `eldoc-documentation-functions'.

CB is the callback provided to members of ElDoc documentation
functions.

https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/Extra.html#Lean.Lsp.PlainTermGoal"
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
          (list
           (concat
            ;; Use wording from Lean documentation:
            ;; https://github.com/leanprover/lean4/blob/6fce8f7d5cd18a4419bca7fd51780c71c9b1cc5a/src/Lean/Data/Lsp/Extra.lean#L99
            (propertize "Expected type:\n" 'face 'nael-eldoc-title)
            doc)
           :echo "")
        (list nil)))))
  t)

(defun nael-eglot-when-server-initialized (_)
  "Disable the `workspace/didChangeConfiguration' notification.

Because it makes Lean language server error."
  (interactive)
  (remove-hook 'eglot-connect-hook
               'eglot-signal-didChangeConfiguration 'local))

(defun nael-eglot-when-managed ()
  "Buffer-locally setup ElDoc for Nael.

Add `nael-eglot-plain-goal-eldoc-function' and
`nael-eglot-plain-term-goal-eldoc-function' to
`eldoc-documentation-functions'.  And use compose-strategy for ElDoc,
see `eldoc-documentation-strategy'."
  (interactive)
  (setq-local eldoc-documentation-strategy
              #'eldoc-documentation-compose)
  (add-hook 'eldoc-documentation-functions
            #'nael-eglot-plain-goal-eldoc-function nil 'local)
  (add-hook 'eldoc-documentation-functions
            #'nael-eglot-plain-term-goal-eldoc-function nil 'local))

;;;; Mode:

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
  ;; Font-lock:
  (setq-local font-lock-defaults
              nael-font-lock-defaults)
  ;; Compile:
  (setq-local compilation-mode-font-lock-keywords
              nil)
  (setq-local compile-command
              "lake build ")
  ;; Flymake:
  (setq-local next-error-function
              #'flymake-goto-next-error)
  ;; Eglot:
  (add-hook 'eglot-server-initialized-hook
            #'nael-eglot-when-server-initialized nil 'local)
  (add-hook 'eglot-managed-mode-hook
            #'nael-eglot-when-managed nil 'local))

;;;; Project:

(add-to-list 'project-vc-extra-root-markers
             "lakefile.lean")

;;;; File extension:

(setf (alist-get "\\.lean\\'" auto-mode-alist nil nil #'equal)
      'nael-mode)

(setf (alist-get "lean" markdown-code-lang-modes nil nil #'equal)
      'nael-mode)

;;;; End:

(provide 'nael)

;;; nael.el ends here
