;;; cobol-ts-mode.el --- Tree-sitter support for COBOL  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  User

;; Author: User
;; Keywords: languages, cobol, tree-sitter

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This mode provides tree-sitter support for COBOL.
;; It requires Emacs 29 or later.

;;; Code:

(require 'treesit)
(require 'rx)

(defgroup cobol-ts nil
  "Tree-sitter support for COBOL."
  :group 'languages
  :prefix "cobol-ts-")

(defcustom cobol-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `cobol-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'cobol-ts)

(defvar cobol-ts--treesit-settings
  '((cobol
     (url "https://github.com/yutaro-sakamoto/tree-sitter-cobol")
     (revision "main")
     (source-dir "src")))
  "Tree-sitter source settings for COBOL.")

;; The grammar defines the language as 'COBOL' (uppercase), so the exported
;; symbol is 'tree_sitter_COBOL', but Emacs expects 'tree_sitter_cobol'.
(add-to-list 'treesit-load-name-override-list
             '(cobol "libtree-sitter-cobol" "tree_sitter_COBOL"))

;;;###autoload
(defun cobol-ts-install-grammar ()
  "Install the COBOL grammar for tree-sitter."
  (interactive)
  (add-to-list 'treesit-language-source-alist
               `(cobol . ,(alist-get 'cobol cobol-ts--treesit-settings)))
  (treesit-install-language-grammar 'cobol))

(defun cobol-ts--fontify-text (node text override)
  "Fontify TEXT within NODE with `font-lock-keyword-face`."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node))
        (case-fold-search t))
    (save-excursion
      (goto-char start)
      (while (search-forward text end t)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0)))
          ;; Check if we are inside a string or comment
          (let ((node-at (treesit-node-at match-start)))
            (unless (and (member (treesit-node-type node-at) '("string" "comment" "comment_entry"))
                         (<= (treesit-node-start node-at) match-start))
              (treesit-fontify-with-override
               match-start match-end 'font-lock-keyword-face override))))))))

(defun cobol-ts-fontify-keyword (node override start end &rest _)
  "Fontify keywords for NODE."
  (let ((type (treesit-node-type node)))
    (pcase type
      ;; Divisions
      ("identification_division"
       (cobol-ts--fontify-text node "IDENTIFICATION" override)
       (cobol-ts--fontify-text node "DIVISION" override)
       (cobol-ts--fontify-text node "PROGRAM-ID" override))
      ("environment_division"
       (cobol-ts--fontify-text node "ENVIRONMENT" override)
       (cobol-ts--fontify-text node "DIVISION" override))
      ("data_division"
       (cobol-ts--fontify-text node "DATA" override)
       (cobol-ts--fontify-text node "DIVISION" override))
      ("procedure_division"
       (cobol-ts--fontify-text node "PROCEDURE" override)
       (cobol-ts--fontify-text node "DIVISION" override))
      
      ;; Sections
      ("configuration_section"
       (cobol-ts--fontify-text node "CONFIGURATION" override)
       (cobol-ts--fontify-text node "SECTION" override))
      ("working_storage_section"
       (cobol-ts--fontify-text node "WORKING-STORAGE" override)
       (cobol-ts--fontify-text node "SECTION" override))
      ("file_section"
       (cobol-ts--fontify-text node "FILE" override)
       (cobol-ts--fontify-text node "SECTION" override))
      ("linkage_section"
       (cobol-ts--fontify-text node "LINKAGE" override)
       (cobol-ts--fontify-text node "SECTION" override))
      ("section_header"
       (cobol-ts--fontify-text node "SECTION" override))

      ("input_output_section"
       (cobol-ts--fontify-text node "INPUT-OUTPUT" override)
       (cobol-ts--fontify-text node "SECTION" override)
       (cobol-ts--fontify-text node "FILE-CONTROL" override)
       (cobol-ts--fontify-text node "I-O-CONTROL" override))

      ;; Paragraphs
      ("source_computer_paragraph" (cobol-ts--fontify-text node "SOURCE-COMPUTER" override))
      ("object_computer_paragraph" (cobol-ts--fontify-text node "OBJECT-COMPUTER" override))
      ("author_section" (cobol-ts--fontify-text node "AUTHOR" override))

      ;; Statements
      ("display_statement" (cobol-ts--fontify-text node "DISPLAY" override))
      ("stop_statement" 
       (cobol-ts--fontify-text node "STOP" override)
       (cobol-ts--fontify-text node "RUN" override))

      ("if_header" (cobol-ts--fontify-text node "IF" override))
      ("else_header" (cobol-ts--fontify-text node "ELSE" override))
      ("END_IF" (cobol-ts--fontify-text node "END-IF" override))
      ("END_PERFORM" (cobol-ts--fontify-text node "END-PERFORM" override))
      ("END_EVALUATE" (cobol-ts--fontify-text node "END-EVALUATE" override))

      ("perform_statement_loop"
       (cobol-ts--fontify-text node "PERFORM" override)
       (cobol-ts--fontify-text node "UNTIL" override)
       (cobol-ts--fontify-text node "VARYING" override)
       (cobol-ts--fontify-text node "FROM" override)
       (cobol-ts--fontify-text node "BY" override)
       (cobol-ts--fontify-text node "TIMES" override))
      ("perform_statement_call_proc"
       (cobol-ts--fontify-text node "PERFORM" override)
       (cobol-ts--fontify-text node "THRU" override))
      ("move_statement"
       (cobol-ts--fontify-text node "MOVE" override)
       (cobol-ts--fontify-text node "TO" override)
       (cobol-ts--fontify-text node "CORRESPONDING" override))
      ("subtract_statement"
       (cobol-ts--fontify-text node "SUBTRACT" override)
       (cobol-ts--fontify-text node "FROM" override)
       (cobol-ts--fontify-text node "GIVING" override))
      ("multiply_statement"
       (cobol-ts--fontify-text node "MULTIPLY" override)
       (cobol-ts--fontify-text node "BY" override)
       (cobol-ts--fontify-text node "GIVING" override))

      ("open_statement"
       (cobol-ts--fontify-text node "OPEN" override)
       (cobol-ts--fontify-text node "INPUT" override)
       (cobol-ts--fontify-text node "OUTPUT" override)
       (cobol-ts--fontify-text node "I-O" override)
       (cobol-ts--fontify-text node "EXTEND" override))
      ("read_statement"
       (cobol-ts--fontify-text node "READ" override)
       (cobol-ts--fontify-text node "NEXT" override)
       (cobol-ts--fontify-text node "RECORD" override)
       (cobol-ts--fontify-text node "INTO" override)
       (cobol-ts--fontify-text node "AT" override)
       (cobol-ts--fontify-text node "END" override)
       (cobol-ts--fontify-text node "NOT" override)
       (cobol-ts--fontify-text node "INVALID" override)
       (cobol-ts--fontify-text node "KEY" override))
      ("rewrite_statement" (cobol-ts--fontify-text node "REWRITE" override))
      
      ;; Clauses
      ("picture_clause"
       (cobol-ts--fontify-text node "PICTURE" override)
       (cobol-ts--fontify-text node "PIC" override))
      ("value_clause" (cobol-ts--fontify-text node "VALUE" override))
      ("usage_clause"
       (cobol-ts--fontify-text node "USAGE" override)
       (cobol-ts--fontify-text node "COMP" override)
       (cobol-ts--fontify-text node "COMP-3" override)
       (cobol-ts--fontify-text node "BINARY" override)
       (cobol-ts--fontify-text node "DISPLAY" override)
       (cobol-ts--fontify-text node "INDEX" override)
       (cobol-ts--fontify-text node "PACKED-DECIMAL" override))

      )))

(defvar cobol-ts--font-lock-settings
  (treesit-font-lock-rules
   :language 'cobol
   :feature 'comment
   '((comment) @font-lock-comment-face
     (comment_entry) @font-lock-comment-face)

   :language 'cobol
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'cobol
   :feature 'number
   '((number) @font-lock-number-face
     (integer) @font-lock-number-face
     (level_number) @font-lock-number-face)

   :language 'cobol
   :feature 'keyword

   '(((identification_division) @cobol-ts-fontify-keyword)
     ((environment_division) @cobol-ts-fontify-keyword)
     ((data_division) @cobol-ts-fontify-keyword)
     ((procedure_division) @cobol-ts-fontify-keyword)
     ((configuration_section) @cobol-ts-fontify-keyword)
     ((working_storage_section) @cobol-ts-fontify-keyword)
     ((section_header) @cobol-ts-fontify-keyword)
     ((source_computer_paragraph) @cobol-ts-fontify-keyword)
     ((object_computer_paragraph) @cobol-ts-fontify-keyword)
     ((author_section) @cobol-ts-fontify-keyword)
     ((display_statement) @cobol-ts-fontify-keyword)
     ((stop_statement) @cobol-ts-fontify-keyword)
     ((if_header) @cobol-ts-fontify-keyword)
     ((else_header) @cobol-ts-fontify-keyword)
     ((END_IF) @cobol-ts-fontify-keyword)
     ((perform_statement_loop) @cobol-ts-fontify-keyword)
     ((END_PERFORM) @cobol-ts-fontify-keyword)
     ((picture_clause) @cobol-ts-fontify-keyword)
     ((value_clause) @cobol-ts-fontify-keyword))

   :language 'cobol
   :feature 'definition
    '((program_name) @font-lock-function-name-face
      (section_header) @font-lock-type-face
      (paragraph_header) @font-lock-function-name-face
      (data_description (entry_name) @font-lock-variable-name-face)
      (select_statement file_name: (_) @font-lock-variable-name-face))

   :language 'cobol
   :feature 'type
   '((picture_clause (_) @font-lock-type-face)
     (usage_clause (_) @font-lock-type-face))
   )
  "Tree-sitter font-lock settings for `cobol-ts-mode'.")

(defvar cobol-ts--indent-rules
  `((cobol
     ((parent-is "source_text") column-0 0)
     ((node-is "identification_division") parent-bol 0)
     ((node-is "environment_division") parent-bol 0)
     ((node-is "data_division") parent-bol 0)
     ((node-is "procedure_division") parent-bol 0)
     ((node-is "section_header") parent-bol 0)
     ((node-is "paragraph_header") parent-bol 0)
     
     ;; Inside divisions
     ((parent-is "identification_division") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "environment_division") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "data_division") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "procedure_division") parent-bol cobol-ts-mode-indent-offset)
     
     ;; Inside sections
     ((parent-is "working_storage_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "linkage_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "file_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "section") parent-bol cobol-ts-mode-indent-offset)
     
     ;; Inside paragraphs
     ((parent-is "paragraph") parent-bol cobol-ts-mode-indent-offset)

     ;; Data descriptions
     ((parent-is "data_description") parent-bol cobol-ts-mode-indent-offset)
     
     ;; Statements (nested)
     ((parent-is "perform_statement") prev-line cobol-ts-mode-indent-offset)
     ((parent-is "perform_statement_loop") prev-line cobol-ts-mode-indent-offset)
     ((parent-is "if_statement") prev-line cobol-ts-mode-indent-offset)
     ((parent-is "evaluate_statement") prev-line cobol-ts-mode-indent-offset)
     ((parent-is "read_statement") prev-line cobol-ts-mode-indent-offset)
     ((parent-is "write_statement") prev-line cobol-ts-mode-indent-offset)
     ((parent-is "display_statement") prev-line cobol-ts-mode-indent-offset)
     ((parent-is "sentence") parent-bol 0)
     ))
  "Tree-sitter indentation rules for `cobol-ts-mode'.")



(defvar cobol-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- "w" table)
    table)
  "Syntax table for `cobol-ts-mode'.")

;;;###autoload
(define-derived-mode cobol-ts-mode prog-mode "COBOL-TS"
  "Major mode for editing COBOL files using tree-sitter."
  :group 'cobol-ts
  
  (unless (treesit-ready-p 'cobol)
    (error "Tree-sitter for COBOL isn't available"))

  (treesit-parser-create 'cobol)

  ;; Comments
  (setq-local comment-start "*")
  (setq-local comment-start-skip "\\*+\\s-*")

  ;; Font-lock
  (setq-local treesit-font-lock-settings cobol-ts--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type number)
                () ()))

  ;; Indentation
  (setq-local treesit-simple-indent-rules cobol-ts--indent-rules)

  ;; Navigation
  (setq-local treesit-defun-type-regexp (rx (or "division" "section" "paragraph")))

  (treesit-major-mode-setup))
  


(add-to-list 'auto-mode-alist '("\\.cob\\'" . cobol-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cbl\\'" . cobol-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpy\\'" . cobol-ts-mode))

(provide 'cobol-ts-mode)
;;; cobol-ts-mode.el ends here
