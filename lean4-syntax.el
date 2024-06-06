;;; lean4-syntax.el --- Syntax table for lean4-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; This library defines syntaxes for `lean4-mode'.

;;; Code:

(require 'rx)

(defconst lean4-keywords1
  '(
    "abbrev" "at" "attribute" "attributes" "axiom" "begin" "break"
    "builtin_initialize" "by" "cases" "catch" "class" "constant"
    "continue" "declare_syntax_cat" "def" "deriving" "do" "elab"
    "else" "end" "example" "exists" "export" "extends" "finally" "for"
    "forall" "from" "fun" "generalizing" "have" "hide" "hiding" "if"
    "import" "in" "include" "induction" "inductive" "infix" "infixl"
    "infixr" "init_quot" "initialize" "instance" "lemma" "let" "local"
    "macro" "macro_rules" "match" "match_syntax" "mut" "mutual"
    "namespace" "nomatch" "noncomputable" "notation" "open" "partial"
    "postfix" "precedence" "prefix" "prelude" "private" "protected"
    "raw" "rec" "register_builtin_option" "renaming" "return"
    "run_cmd" "scoped" "section" "set_option" "show" "structure"
    "suffices" "syntax" "then" "theorem" "this" "try" "unif_hint"
    "universe" "universes" "unless" "unsafe" "using"
    "using_well_founded" "variable" "variables" "where" "with")
  "Lean keywords ending with `word' (not symbol).")
(defconst lean4-keywords1-regexp
  (eval `(rx word-start (or ,@lean4-keywords1) word-end)))
(defconst lean4-constants
  '("!" "#" "$" "&&" "*" "+" "+c" "+f" "+n" "-" "->" "/" "/" "/\\"
    ":=" "<" "<->" "<=" "=" "==" ">" ">=" "@" "\\/" "^c" "||" "~" "¬"
    "×" "×c" "×f" "×n" "Π" "Σ" "λ" "⁻¹" "ℂ" "ℕ" "ℕ₋₂" "ℚ" "ℝ" "ℤ" "→"
    "↔" "∀" "∃" "∘" "∘1nf" "∘f" "∘f1n" "∘fi" "∘fn" "∘fn1" "∘n" "∘n1f"
    "∘nf" "∧" "∨" "∼" "≃" "≃c" "≅" "≅c" "≠" "≡" "≤" "≥" "▸" "◾" "◾o"
    "⬝" "⬝e" "⬝h" "⬝hp" "⬝i" "⬝o" "⬝op" "⬝ph" "⬝po" "⬝pv" "⬝r" "⬝v"
    "⬝vp" "𝔸")
  "Lean constants.")
(defconst lean4-constants-regexp (regexp-opt lean4-constants))
(defconst lean4-numerals-regexp
  (rx word-start
      (one-or-more digit)
      (optional (and "." (zero-or-more digit)))
      word-end))

(defconst lean4-warnings '("sorry") "Lean warnings.")
(defconst lean4-warnings-regexp
  (eval `(rx word-start (or ,@lean4-warnings) word-end)))
(defconst lean4-debugging '("unreachable" "panic" "assert" "dbgTrace")
  "Lean debugging.")
(defconst lean4-debugging-regexp
  (eval `(rx word-start (or ,@lean4-debugging))))

(defconst lean4-syntax-table
  (let ((st (make-syntax-table)))
    ;; Matching parens
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)

    ;; comment
    (modify-syntax-entry ?/ ". 14nb" st)
    (modify-syntax-entry ?- ". 123" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?« "<" st)
    (modify-syntax-entry ?» ">" st)

    ;; Word constituent
    (dolist
        (chr
         '( ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
            ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
            ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
            ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z))
      (modify-syntax-entry chr "w" st))
    (dolist
        (chr
         '( ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
      (modify-syntax-entry chr "w" st))
    (dolist
        (chr
         '( ?α ?β ?γ ?δ ?ε ?ζ ?η ?θ ?ι ?κ ;;?λ
            ?μ ?ν ?ξ ?ο ?π ?ρ ?ς ?σ ?τ ?υ
            ?φ ?χ ?ψ ?ω))
      (modify-syntax-entry chr "w" st))
    (dolist
        (chr
         '( ?ϊ ?ϋ ?ό ?ύ ?ώ ?Ϗ ?ϐ ?ϑ ?ϒ ?ϓ ?ϔ ?ϕ ?ϖ
            ?ϗ ?Ϙ ?ϙ ?Ϛ ?ϛ ?Ϝ ?ϝ ?Ϟ ?ϟ ?Ϡ ?ϡ ?Ϣ ?ϣ
            ?Ϥ ?ϥ ?Ϧ ?ϧ ?Ϩ ?ϩ ?Ϫ ?ϫ ?Ϭ ?ϭ ?Ϯ ?ϯ ?ϰ
            ?ϱ ?ϲ ?ϳ ?ϴ ?ϵ ?϶ ?Ϸ ?ϸ ?Ϲ ?Ϻ ?ϻ))
      (modify-syntax-entry chr "w" st))
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
      (modify-syntax-entry chr "w" st))
    (dolist
        (chr
         '( ?℀ ?℁ ?ℂ ?℃ ?℄ ?℅ ?℆ ?ℇ ?℈ ?℉ ?ℊ ?ℋ ?ℌ ?ℍ ?ℎ
            ?ℏ ?ℐ ?ℑ ?ℒ ?ℓ ?℔ ?ℕ ?№ ?℗ ?℘ ?ℙ ?ℚ ?ℛ ?ℜ ?ℝ
            ?℞ ?℟ ?℠ ?℡ ?™ ?℣ ?ℤ ?℥ ?Ω ?℧ ?ℨ ?℩ ?K ?Å ?ℬ
            ?ℭ ?℮ ?ℯ ?ℰ ?ℱ ?Ⅎ ?ℳ ?ℴ ?ℵ ?ℶ ?ℷ ?ℸ ?ℹ ?℺ ?℻
            ?ℼ ?ℽ ?ℾ ?ℿ ?⅀ ?⅁ ?⅂ ?⅃ ?⅄ ?ⅅ ?ⅆ ?ⅇ ?ⅈ ?ⅉ ?⅊
            ?⅋ ?⅌ ?⅍ ?ⅎ ?⅏))
      (modify-syntax-entry chr "w" st))
    (dolist
        (chr
         '( ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀
            ?ₐ ?ₑ ?ₒ ?ₓ ?ₔ ?ₕ ?ₖ ?ₗ ?ₘ ?ₙ ?ₚ ?ₛ ?ₜ
            ?' ?_ ?! ??))
      (modify-syntax-entry chr "w" st))

    ;; Lean operator chars
    (dolist
        (chr (string-to-list "#$%&*+<=>@^|~:"))
      (modify-syntax-entry chr "." st))

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    st))

(defconst lean4-font-lock-defaults
  (list
   (list
    (list
     (rx word-start "attribute" word-end
         (zero-or-more whitespace)
         (group
          (one-or-more "[" (zero-or-more (not (any "]")))
                       "]" (zero-or-more whitespace))))
     '(1 'font-lock-preprocessor-face))
    (list
     (rx (group "@[" (zero-or-more (not (any "]"))) "]"))
     '(1 'font-lock-preprocessor-face))
    (list
     (rx
      (group "#"
             (or "eval" "print" "reduce" "help" "check" "lang"
                 "check_failure" "synth")))
     '(1 'font-lock-keyword-face))

    ;; mutual definitions "names"
    (list
     (rx word-start "mutual" word-end
         (zero-or-more whitespace)
         word-start (or "inductive" "definition" "def") word-end
         (group (zero-or-more (not (any " \t\n\r{([,")))
                (zero-or-more (zero-or-more whitespace) ","
                              (zero-or-more whitespace)
                              (not (any " \t\n\r{([,")))))
     '(1 'font-lock-function-name-face))

    ;; declarations
    (list
     (rx word-start
         (group
          (or "inductive"
              (group "class" (zero-or-more whitespace) "inductive")
              "instance" "structure" "class" "theorem" "axiom"
              "lemma" "definition" "def" "constant"))
         word-end (zero-or-more whitespace)
         (group
          (zero-or-more "{" (zero-or-more (not (any "}")))
                        "}" (zero-or-more whitespace)))
         (zero-or-more whitespace)
         (group (zero-or-more (not (any " \t\n\r{([")))))
     '(4 'font-lock-function-name-face))

    ;; Constants which have a keyword as subterm
    (cons (rx (or "∘if")) 'font-lock-constant-face)

    ;; Keywords
    (list
     "\\(set_option\\)[ \t]*\\([^ \t\n]*\\)"
     '(2 'font-lock-constant-face))
    (cons 'lean4-keywords1-regexp 'font-lock-keyword-face)
    (list
     (rx word-start (group "example") ".")
     '(1 'font-lock-keyword-face))
    (cons (rx (or "∎")) 'font-lock-keyword-face)

    ;; Types
    (cons
     (rx word-start
         (or "Prop" "Type" "Type*" "Sort" "Sort*") symbol-end)
     'font-lock-type-face)
    (list
     (rx word-start (group (or "Prop" "Type" "Sort")) ".")
     '(1 'font-lock-type-face))

    ;; String
    (cons "\"[^\"]*\"" 'font-lock-string-face)

    ;; ;; Constants
    (cons 'lean4-constants-regexp 'font-lock-constant-face)
    (cons 'lean4-numerals-regexp  'font-lock-constant-face)

    ;; place holder
    (cons (rx symbol-start "_" symbol-end)
          'font-lock-preprocessor-face)

    ;; warnings
    (cons 'lean4-warnings-regexp  'font-lock-warning-face)
    (cons 'lean4-debugging-regexp 'font-lock-warning-face)

    ;; escaped identifiers
    (list
     (rx (and (group "«")
              (group (one-or-more (not (any "»"))))
              (group "»")))
     '(1 font-lock-comment-face t)
     '(2 nil t)
     '(3 font-lock-comment-face t)))))

;; Syntax Highlighting for Lean Info Mode
(defconst lean4-info-font-lock-defaults
  (let ((new-entries
         `(;; Please add more after this:
           (,(rx (group
                  (+ symbol-start
                     (+
                      (or word (char ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀)))
                     symbol-end (* white)))
                 ":")
            (1 'font-lock-variable-name-face))
           (,(rx white ":" white)
            . 'font-lock-keyword-face)
           (,(rx "⊢" white)
            . 'font-lock-keyword-face)
           (,(rx "[" (group "stale") "]")
            (1 'font-lock-warning-face))
           (,(rx line-start "No Goal" line-end)
            . 'font-lock-constant-face)))
        (inherited-entries (car lean4-font-lock-defaults)))
    `(,(append new-entries inherited-entries))))

(provide 'lean4-syntax)

;;; lean4-syntax.el ends here
