;;; nael-syntax.el --- Syntax table for nael-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; This library defines syntaxes for `nael-mode'.

;;; Code:

(require 'rx)

(defconst nael-keywords1
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

(defconst nael-keywords1-regexp
  (rx-to-string `(seq word-start (or ,@nael-keywords1) word-end)))

(defconst nael-constants
  '("!" "#" "$" "&&" "*" "+" "+c" "+f" "+n" "-" "->" "/" "/" "/\\"
    ":=" "<" "<->" "<=" "=" "==" ">" ">=" "@" "\\/" "^c" "||" "~" "¬"
    "×" "×c" "×f" "×n" "Π" "Σ" "λ" "⁻¹" "ℂ" "ℕ" "ℕ₋₂" "ℚ" "ℝ" "ℤ" "→"
    "↔" "∀" "∃" "∘" "∘1nf" "∘f" "∘f1n" "∘fi" "∘fn" "∘fn1" "∘n" "∘n1f"
    "∘nf" "∧" "∨" "∼" "≃" "≃c" "≅" "≅c" "≠" "≡" "≤" "≥" "▸" "◾" "◾o"
    "⬝" "⬝e" "⬝h" "⬝hp" "⬝i" "⬝o" "⬝op" "⬝ph" "⬝po" "⬝pv" "⬝r" "⬝v"
    "⬝vp" "𝔸")
  "Lean constants.")

(defconst nael-constants-regexp
  (regexp-opt nael-constants))

(defconst nael-numerals-regexp
  (rx word-start
      (one-or-more digit)
      (optional (and "." (zero-or-more digit)))
      word-end))

(defconst nael-warnings
  '("sorry")
  "Lean warnings.")

(defconst nael-warnings-regexp
  (rx-to-string `(seq word-start (or ,@nael-warnings) word-end)))

(defconst nael-debugging
  '("unreachable" "panic" "assert" "dbgTrace")
  "Lean debugging.")

(defconst nael-debugging-regexp
  (rx-to-string `(seq word-start (or ,@nael-debugging))))

(defconst nael-font-lock-defaults
  (list
   (list
    (list (rx word-start "attribute" word-end
              (zero-or-more whitespace)
              (group
               (one-or-more "[" (zero-or-more (not (any "]")))
                            "]" (zero-or-more whitespace))))
          '(1 'font-lock-preprocessor-face))
    (list (rx (group "@[" (zero-or-more (not (any "]"))) "]"))
          '(1 'font-lock-preprocessor-face))
    (list (rx (group "#"
                     (or "eval" "print" "reduce" "help" "check" "lang"
                         "check_failure" "synth")))
          '(1 'font-lock-keyword-face))

    ;; mutual definitions "names"
    (list (rx word-start "mutual" word-end
              (zero-or-more whitespace)
              word-start (or "inductive" "definition" "def") word-end
              (group (zero-or-more (not (any " \t\n\r{([,")))
                     (zero-or-more (zero-or-more whitespace) ","
                                   (zero-or-more whitespace)
                                   (not (any " \t\n\r{([,")))))
          '(1 'font-lock-function-name-face))

    ;; declarations
    (list (rx word-start
              (group
               (or "inductive"
                   (group "class" (zero-or-more whitespace)
                          "inductive")
                   "instance" "structure" "class" "theorem"
                   "axiom" "lemma" "definition" "def" "constant"))
              word-end (zero-or-more whitespace)
              (group (zero-or-more "{" (zero-or-more (not (any "}")))
                                   "}" (zero-or-more whitespace)))
              (zero-or-more whitespace)
              (group (zero-or-more (not (any " \t\n\r{([")))))
          '(4 'font-lock-function-name-face))

    ;; Constants which have a keyword as subterm
    (cons (rx (or "∘if"))
          'font-lock-constant-face)

    ;; Keywords
    (list "\\(set_option\\)[ \t]*\\([^ \t\n]*\\)"
          '(2 'font-lock-constant-face))
    (cons nael-keywords1-regexp
          'font-lock-keyword-face)
    (list (rx word-start (group "example") ".")
          '(1 'font-lock-keyword-face))
    (cons (rx (or "∎"))
          'font-lock-keyword-face)

    ;; Types
    (cons (rx word-start (or "Prop" "Type" "Type*" "Sort" "Sort*")
              symbol-end)
          'font-lock-type-face)
    (list (rx word-start (group (or "Prop" "Type" "Sort")) ".")
          '(1 'font-lock-type-face))

    ;; String
    (cons "\"[^\"]*\"" 'font-lock-string-face)

    ;; ;; Constants
    (cons nael-constants-regexp 'font-lock-constant-face)
    (cons nael-numerals-regexp  'font-lock-constant-face)

    ;; place holder
    (cons (rx symbol-start "_" symbol-end)
          'font-lock-preprocessor-face)

    ;; warnings
    (cons nael-warnings-regexp  'font-lock-warning-face)
    (cons nael-debugging-regexp 'font-lock-warning-face)

    ;; escaped identifiers
    (list (rx (and (group "«") (group (one-or-more (not (any "»"))))
                   (group "»")))
          '(1 font-lock-comment-face t)
          '(2 nil t)
          '(3 font-lock-comment-face t)))))

(defconst nael-info-font-lock-defaults
  (list
   (append
    (list
     (list (rx (group
                (+ symbol-start (+ (or word (in (?₀ . ?₉))))
                   symbol-end (* white)))
               ":")
           '(1 font-lock-variable-name-face))
     (cons (rx white ":" white) 'font-lock-keyword-face)
     (cons (rx "⊢" white) 'font-lock-keyword-face)
     (list (rx "[" (group "stale") "]") '(1 font-lock-warning-face))
     (cons (rx bol "No Goal" eol) 'font-lock-constant-face))
    (car nael-font-lock-defaults))))

(provide 'nael-syntax)

;;; nael-syntax.el ends here
