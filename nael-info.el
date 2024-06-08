;;; nael-info.el --- Emacs mode for Lean theorem prover -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Microsoft Corporation. All rights reserved.
;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: Apache-2.0, GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Some parts of this source code are licensed under Apache-2.0,
;; others are licensed under GNU General Public License version 3 or
;; later.

;;; Commentary:

;; This library provides an advanced LSP feature for `nael-mode'.

;;; Code:

(require 'cl-lib)
(require 'eglot)

(require 'magit-section)

(require 'nael-syntax)

(defgroup nael-info nil
  "Lean Info."
  :group 'nael)

(defcustom nael-highlight-inaccessible-names t
  "Use font to highlight inaccessible names.

Set this variable to `t' to highlight inaccessible names in the info
display using `font-lock-comment-face' instead of the `✝' suffix used
by Lean."
  :group 'nael :type 'boolean)

(defvar nael-info-font-lock-defaults
  (list
   (append
    (list
     (list (rx
            (group
             (one-or-more
              symbol-start (one-or-more (any "₀-₉" word)) symbol-end
              (zero-or-more space)))
            ":")
           '(1 font-lock-variable-name-face))
     (cons (rx space ":" space)
           'font-lock-keyword-face)
     (cons (rx "⊢" space)
           'font-lock-keyword-face)
     (list (rx "[" (group "stale") "]")
           '(1 font-lock-warning-face))
     (cons (rx line-start "No Goal" line-end)
           'font-lock-constant-face))
    (car nael-font-lock-defaults))))

;;;###autoload
(define-derived-mode nael-info-mode prog-mode "Nael-Info"
  "Helper mode for Nael info buffer.

This mode is only used in temporary buffers, for fontification."
  :syntax-table nael-syntax-table
  :group 'nael
  (set (make-local-variable 'font-lock-defaults)
       nael-info-font-lock-defaults))

(defun nael-ensure-info-buffer (buffer)
  "Create BUFFER if it does not exist.

Also choose settings used for the *Lean Goal* buffer."
  (unless (get-buffer buffer)
    (with-current-buffer (get-buffer-create buffer)
      (magit-section-mode)
      (buffer-disable-undo)
      (add-hook 'eldoc-documentation-functions
                #'nael-info-eldoc-function nil t)
      (eldoc-mode)
      (set-input-method "Lean")
      (set-syntax-table nael-syntax-table)
      (setq buffer-read-only t))))

(defun nael-toggle-info-buffer (buffer)
  "Create or delete BUFFER.

The buffer is supposed to be the `*Lean Goal*' buffer."
  (if-let ((window (get-buffer-window buffer)))
      (quit-window nil window)
    (nael-ensure-info-buffer buffer)
    (display-buffer buffer)))

(defun nael-info-buffer-active (buffer)
  "Check if given info BUFFER should show info for current buffer."
  (and
   ;; info buffer visible (on any frame)
   (get-buffer-window buffer t)
   ;; current window of current buffer is selected (i.e., in focus)
   (eq (current-buffer) (window-buffer))
   ;; current buffer is visiting a file
   buffer-file-name))

(defconst nael-info-buffer-name "*Nael Goal*")

(defvar nael-info--goals nil)
(defvar nael-info--term-goal nil)

(defun nael-info--diagnostics ()
  (nreverse
   (cl-loop
    for diag in (flymake-diagnostics)
    when (cdr (assoc 'eglot-lsp-diag (eglot--diag-data diag)))
    collect it)))

(defun nael-info--diagnostic-start (diagnostic)
  (eglot--dbind ((Range) start) (cl-getf diagnostic :fullRange)
    (eglot--dbind ((Position) line) start
      line)))

(defun nael-info--diagnostic-end (diagnostic)
  (eglot--dbind ((Range) end) (cl-getf diagnostic :fullRange)
    (eglot--dbind ((Position) line) end
      line)))

(defun nael-info--fontify-string (s)
  (with-temp-buffer
    (nael-info-mode)
    (insert s)
    (font-lock-ensure)
    (buffer-string)))

(defun nael-mk-message-section (value caption errors)
  "Add a section with caption CAPTION and contents ERRORS."
  (when errors
    (magit-insert-section (magit-section value)
      (magit-insert-heading caption)
      (magit-insert-section-body
        (dolist (e errors)
          (magit-insert-section (magit-section)
            (magit-insert-section-body
              (eglot--dbind ((Diagnostic) message range) e
                (eglot--dbind ((Range) start) range
                  (eglot--dbind ((Position) line character) start
                    (magit-insert-heading
                      (format "%d:%d" (1+ line) character))
                    (insert message "\n")))))))))))

(defun nael-info-buffer-redisplay ()
  (let ((inhibit-message t)
        (inhibit-read-only t))
    (when (nael-info-buffer-active nael-info-buffer-name)
      (let* ((deactivate-mark) ; keep transient mark
             (line
              (save-restriction
                (widen)
                (1- (line-number-at-pos nil t))))
             (errors
              (seq-sort-by #'nael-info--diagnostic-end #'<
                           (nael-info--diagnostics)))
             (errors-above
              (seq-take-while
               (lambda (e) (< (nael-info--diagnostic-end e) line))
               errors))
             (errors
              (seq-drop errors (seq-length errors-above)))
             (errors-here
              (seq-take-while
               (lambda (e) (<= (nael-info--diagnostic-end e) line))
               errors))
             (errors-below
              (seq-drop errors (seq-length errors-here))))
        (with-current-buffer nael-info-buffer-name
          (erase-buffer)
          (magit-insert-section (magit-section 'root)
            (when nael-info--goals
              (magit-insert-section (magit-section 'goals)
                (magit-insert-heading "Goals:")
                (let ((goals nael-info--goals))
                  (magit-insert-section-body
                    (if (> (length goals) 0)
                        (seq-doseq (g goals)
                          (magit-insert-section (magit-section)
                            (insert
                             (nael-info--fontify-string g) "\n\n")))
                      (insert "goals accomplished\n\n"))))))
            (when nael-info--term-goal
              (magit-insert-section (magit-section 'term-goal)
                (magit-insert-heading "Expected type:")
                (let ((term-goal nael-info--term-goal))
                  (magit-insert-section-body
                    (insert
                     (nael-info--fontify-string term-goal)
                     "\n\n")))))
            (nael-mk-message-section
             'errors-here "Messages here:" errors-here)
            (nael-mk-message-section
             'errors-below "Messages below:" errors-below)
            (nael-mk-message-section
             'errors-above "Messages above:" errors-above)
            (when nael-highlight-inaccessible-names
              (goto-char 1)
              (save-match-data
                (while
                    (re-search-forward
                     "\\(\\sw+\\)✝\\([¹²³⁴-⁹⁰]*\\)" nil t)
                  (replace-match
                   (propertize
                    (concat (match-string-no-properties 1)
                            (match-string-no-properties 2))
                    'font-lock-face 'font-lock-comment-face)
                   'fixedcase 'literal))))))))))

(defcustom nael-info-plain t
  "If `t', then use plain text for info buffer.

If `nil', then enable \"hover docs\" in the info buffer.  This is an
experimental feature that requires further testing."
  :type
  '(choice
    (const :tag "Plain text" t)
    (const :tag "Hover docs" nil))
  :group 'nael)

(defvar nael--rpc-server nil)
(defvar nael--rpc-textDocument nil)
(defvar nael--rpc-position nil)
(defvar nael--rpc-sessionId nil)
(defvar nael--rpc-timer nil)

(defun nael--rpc-connect (&optional buf)
  "Initiate an rpc connection.

This sets the variables nael--rpc-*."
  (unless buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (let*
        ((server (eglot-current-server))
         (textdoc-pos (eglot--TextDocumentPositionParams))
         (uri (plist-get (plist-get textdoc-pos :textDocument) :uri))
         (response
          (jsonrpc-request server :$/lean/rpc/connect `(:uri ,uri)))
         (sessionId (plist-get response :sessionId)))
      (setq nael--rpc-server server)
      (setq nael--rpc-textDocument
            (plist-get textdoc-pos :textDocument))
      (setq nael--rpc-position (plist-get textdoc-pos :position))
      (setq nael--rpc-sessionId sessionId)
      (unless nael--rpc-timer
        (setq nael--rpc-timer
              (run-with-timer 0 5 #'nael-info--rpc-keepalive))))))

(defun nael-info--rpc-keepalive ()
  (when nael--rpc-server
    (condition-case nil
        (jsonrpc-notify
         nael--rpc-server
         :$/lean/rpc/keepAlive
         `(:uri ,(plist-get nael--rpc-textDocument :uri)
                :sessionId ,nael--rpc-sessionId))
      (error (cancel-timer nael--rpc-timer)
             (setq nael--rpc-timer nil)
             (setq nael--rpc-server nil)
             (setq nael--rpc-textDocument nil)
             (setq nael--rpc-position nil)
             (setq nael--rpc-sessionId nil)))))

(defun nael-info-parse-goal (goal)
  "Parse GOAL into propertized string."
  (let*
      ((userName (plist-get goal :userName))
       (type (plist-get goal :type))
       (hyps (plist-get goal :hyps))
       (goalPrefix (plist-get goal :goalPrefix))
       (ctx (plist-get goal :ctx))
       (p (plist-get ctx :p)))
    (concat
     (when userName (concat "case " userName "\n"))
     (mapconcat (lambda (hyp)
                  (nael-info-parse-hyp hyp (list p)))
                hyps "\n")
     "\n"
     goalPrefix
     (nael-info-parse-type type (list p)))))

(defun nael-info-parse-hyp (hyp ps)
  "Parse hypothesis HYP into propertized string.
PS is a list of tag IDs."
  (let* ((type (plist-get hyp :type))
         (names (plist-get hyp :names)))
    (concat (mapconcat #'identity names
                       (propertize " " 'nael-p ps))
            (propertize " : " 'nael-p ps)
            (nael-info-parse-type type ps))))

(defun nael-info-parse-type (type ps)
  "Parse TYPE into propertized string.
PS is a list of tag IDs."
  (let* ((tag (plist-get type :tag)))
    (nael-info-parse-tag tag ps)))

(defun nael-info-parse-tag (tag ps)
  "Parse TAG into propertized string.
PS is a list of tag IDs."
  (let* ((tag0 (aref tag 0))
         (info (plist-get tag0 :info))
         (p (plist-get info :p))
         (tag1 (aref tag 1)))
    (nael-info-parse-expr tag1 (cons p ps))))

(defun nael-info-parse-expr (expr &optional ps)
  "Parse EXPR into propertized string.
PS is a list of tag IDs."
  (cond
   ((equal (car expr) :text)
    (if ps
        (propertize (cadr expr) 'nael-p ps)
      (cadr expr)))
   ((equal (car expr) :append)
    (mapconcat (lambda (item)
                 (cond
                  ((equal (car item) :text)
                   (if ps
                       (propertize (cadr item) 'nael-p ps)
                     (cadr item)))
                  ((equal (car item) :tag)
                   (nael-info-parse-tag (cadr item) ps))))
               (cadr expr)))))

(defun nael-info-buffer-refresh ()
  "Refresh the *Lean Goal* buffer."
  (let* ((server (eglot-current-server))
         (buf (current-buffer))
         (goals :none)
         (term-goal :none)
         (handle-response
          (lambda ()
            (when (and (not (eq goals :none))
                       (not (eq term-goal :none))
                       (buffer-live-p buf))
              ;; print goals and term-goal to *DebugInfo* buffer:
              (with-current-buffer (get-buffer-create "*DebugInfo*")
                (erase-buffer)
                (insert (format "goals: %s\n" goals))
                (insert (format "term-goal: %s\n" term-goal)))
              (with-current-buffer buf
                (setq nael-info--goals goals)
                (setq nael-info--term-goal term-goal)
                (nael-info-buffer-redisplay))))))
    (when (and server
               (nael-info-buffer-active nael-info-buffer-name))
      (if nael-info-plain
          (progn
            (jsonrpc-async-request
             server :$/lean/plainGoal
             (eglot--TextDocumentPositionParams)
             :success-fn (lambda (result)
                           (setq goals (cl-getf result :goals))
                           (funcall handle-response)))
            (jsonrpc-async-request
             server :$/lean/plainTermGoal
             (eglot--TextDocumentPositionParams)
             :success-fn (lambda (result)
                           (setq term-goal (cl-getf result :goal))
                           (funcall handle-response))))
        ;; It might be more elegant to do the following once, when we
        ;; switch to a lean buffer, but putting it here seems more
        ;; robust.
        (nael--rpc-connect) ;; sets the variables nael--rpc-*
        (jsonrpc-async-request
         server :$/lean/rpc/call
         (list
          :method "Lean.Widget.getInteractiveGoals"
          :sessionId nael--rpc-sessionId
          :textDocument nael--rpc-textDocument
          :position nael--rpc-position
          :params (list :textDocument nael--rpc-textDocument
                        :position nael--rpc-position))
         :success-fn
         (lambda (result)
           (setq goals
                 (when result
                   (vconcat
                    (mapcar #'nael-info-parse-goal
                            (cl-getf result :goals)))))
           (funcall handle-response)))
        (jsonrpc-async-request
         server :$/lean/rpc/call
         (list
          :method "Lean.Widget.getInteractiveTermGoal"
          :sessionId nael--rpc-sessionId
          :textDocument nael--rpc-textDocument
          :position nael--rpc-position
          :params (list :textDocument nael--rpc-textDocument
                        :position nael--rpc-position))
         :success-fn
         (lambda (result)
           (setq term-goal
                 (when result (nael-info-parse-goal result)))
           (funcall handle-response)))))))

(defun nael-toggle-info ()
  "Show infos at the current point."
  (interactive)
  (nael-toggle-info-buffer nael-info-buffer-name)
  (nael-info-buffer-refresh))

(defun nael-info--widget-region (&optional pos)
  "Return the region of the widget at POS.

POS defaults to the current point."
  (unless pos (setq pos (point)))
  (when-let ((ps (get-text-property pos 'nael-p)))
    (let ((p (car ps))
          (start (point-min))
          (end (point-max)))
      (while (and start
                  (< start (point-max))
                  (not (member p (get-text-property start 'nael-p))))
        (setq start
              (next-single-property-change
               start 'nael-p nil (point-max))))
      (while (and end
                  (> end (point-min))
                  (not (member p (get-text-property end 'nael-p))))
        (setq end
              (previous-single-property-change
               end 'nael-p nil (point-min))))
      (setq end
            (next-single-property-change
             end 'nael-p nil (point-max)))
      (cons start end))))

(defun nael-info-eldoc-function (cb)
  "Eldoc function for info buffer.

CB is the callback function provided by Eldoc."
  (unless nael-info-plain
    (let* ((pos (point))
           (p (car (get-text-property pos 'nael-p))))
      (let* ((region (nael-info--widget-region pos))
             (min (car region))
             (max (cdr region)))
        (when (and min max)
          (pulse-momentary-highlight-region min max)))
      (when (and nael--rpc-server p)
        (jsonrpc-async-request
         nael--rpc-server :$/lean/rpc/call
         (list
          :method (concat "Lean.Widget.InteractiveDiagnostics."
                          "infoToInteractive")
          :sessionId nael--rpc-sessionId
          :textDocument nael--rpc-textDocument
          :position nael--rpc-position
          :params (list :p p))
         :success-fn
         (lambda (result)
           (with-current-buffer
               (get-buffer-create "*DebugInfoResults*")
             (erase-buffer)
             (insert (format "result: %s" result)))
           (let* ((doc (plist-get result :doc))
                  (type (nael-info-parse-type
                         (plist-get result :type)
                         nil))
                  (expr (nael-info-parse-expr
                         (plist-get result :exprExplicit)))
                  (expr-type (and type expr (concat expr " : " type)))
                  (sep (when (and expr-type doc)
                         "\n")))
             (funcall
              cb (concat expr-type sep doc)
              :echo
              (concat
               expr-type sep
               (when doc
                 (substring
                  doc 0 (string-match-p "\n" doc))))))))))))

(provide 'nael-info)

;;; nael-info.el ends here
