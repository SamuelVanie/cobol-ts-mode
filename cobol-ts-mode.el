;;; cobol-ts-mode.el --- Tree-sitter support for COBOL  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  SamuelVanie

;; Author: SamuelVanie
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

;; The grammar defines the language as 'COBOL' (uppercase), so the exported
;; symbol is 'tree_sitter_COBOL', but Emacs expects 'tree_sitter_cobol'.
(add-to-list 'treesit-load-name-override-list
             '(cobol "libtree-sitter-cobol" "tree_sitter_COBOL"))

;;;###autoload
(defun cobol-ts-install-grammar ()
  "Install the COBOL grammar for tree-sitter."
  (interactive)
  (add-to-list 'treesit-language-source-alist
               '(cobol . ("https://github.com/yutaro-sakamoto/tree-sitter-cobol" "main" "src")))
  (treesit-install-language-grammar 'cobol))

(defun cobol-ts--free-format-p ()
  "Check if the current buffer uses free format COBOL.
Returns t if the buffer contains >>SOURCE FORMAT FREE directive."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^>>SOURCE\\s-+\\(?:FORMAT\\s-+\\)?FREE" nil t)))

(defvar cobol-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators and arithmetic symbols as punctuation
    (modify-syntax-entry ?+   "."     table)
    (modify-syntax-entry ?*   "."     table)
    (modify-syntax-entry ?/   "."     table)
    (modify-syntax-entry ?=   "."     table)
    (modify-syntax-entry ?<   "."     table)
    (modify-syntax-entry ?>   "."     table)
    (modify-syntax-entry ?&   "."     table)
    (modify-syntax-entry ?|   "."     table)

    ;; could be in variable's names
    (modify-syntax-entry ?-   "w"     table)
    (modify-syntax-entry ?_ "w" table)
    
    ;; String delimiters - COBOL accepts both single and double quotes
    (modify-syntax-entry ?\"  "\""    table)
    (modify-syntax-entry ?\'  "\""    table)

    ;; Parentheses
    (modify-syntax-entry ?\(  "()"    table)
    (modify-syntax-entry ?\)  ")("    table)

    ;; Period and comma as punctuation
    (modify-syntax-entry ?.   "."     table)
    (modify-syntax-entry ?,   "."     table)
    (modify-syntax-entry ?\;   "."     table)

    table)
  "Syntax table for `cobol-ts-mode'.")

(defvar cobol-ts-mode--indent-rules-free-format
  `((cobol
     ;; Free format: logical indentation only, no column restrictions

     ;; Top-level structure - no indentation
     ((node-is "program_definition") column-0 0)
     ((node-is "source_element") column-0 0)

     ;; Divisions - start at column 0 in free format
     ((node-is "identification_division") parent-bol 0)
     ((node-is "environment_division") parent-bol 0)
     ((node-is "data_division") parent-bol 0)
     ((node-is "procedure_division") parent-bol 0)
     ((node-is "function_division") parent-bol 0)

     ;; Sections - indent from their parent division
     ((parent-is "identification_division") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "environment_division") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "data_division") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "procedure_division") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "function_division") parent-bol cobol-ts-mode-indent-offset)

     ;; Configuration and I/O sections
     ((parent-is "configuration_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "input_output_section") parent-bol cobol-ts-mode-indent-offset)

     ;; Data description sections
     ((parent-is "working_storage_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "file_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "linkage_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "local_storage_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "screen_section") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "report_section") parent-bol cobol-ts-mode-indent-offset)

     ;; Paragraphs in configuration section
     ((parent-is "source_computer_paragraph") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "object_computer_paragraph") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "special_names_paragraph") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "repository_paragraph") parent-bol cobol-ts-mode-indent-offset)

     ;; Data and file descriptions
     ((parent-is "data_description") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "file_description") parent-bol cobol-ts-mode-indent-offset)

     ;; Section headers - align with parent
     ((node-is "section_header") parent-bol 0)
     ((parent-is "section_header") parent-bol cobol-ts-mode-indent-offset)

     ;; Paragraph headers - align with parent
     ((node-is "paragraph_header") parent-bol 0)
     ((parent-is "paragraph_header") parent-bol cobol-ts-mode-indent-offset)

     ;; Perform statements
     ((parent-is "perform_statement_call_proc") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "perform_statement_loop") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "perform_procedure") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "perform_option") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "perform_varying") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "perform_test") parent-bol cobol-ts-mode-indent-offset)

     ;; If-then-else structures
     ((parent-is "if_header") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "else_if_header") parent-bol cobol-ts-mode-indent-offset)
     ((node-is "else_if_header") parent-bol 0)
     ((node-is "else_header") parent-bol 0)
     ((parent-is "else_header") parent-bol cobol-ts-mode-indent-offset)

     ;; Evaluate (COBOL's switch/case)
     ((parent-is "evaluate_header") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "evaluate_subject") parent-bol cobol-ts-mode-indent-offset)
     ((node-is "when") parent-bol 0)
     ((parent-is "when") parent-bol cobol-ts-mode-indent-offset)
     ((node-is "when_other") parent-bol 0)
     ((parent-is "when_other") parent-bol cobol-ts-mode-indent-offset)

     ;; Search statement
     ((parent-is "search_statement") parent-bol cobol-ts-mode-indent-offset)

     ;; Exception handlers for I/O operations
     ((node-is "at_end") parent-bol 0)
     ((parent-is "at_end") parent-bol cobol-ts-mode-indent-offset)
     ((node-is "not_at_end") parent-bol 0)
     ((parent-is "not_at_end") parent-bol cobol-ts-mode-indent-offset)
     ((node-is "invalid_key") parent-bol 0)
     ((parent-is "invalid_key") parent-bol cobol-ts-mode-indent-offset)
     ((node-is "not_invalid_key") parent-bol 0)
     ((parent-is "not_invalid_key") parent-bol cobol-ts-mode-indent-offset)
     ((node-is "eop") parent-bol 0)
     ((parent-is "eop") parent-bol cobol-ts-mode-indent-offset)
     ((node-is "not_eop") parent-bol 0)
     ((parent-is "not_eop") parent-bol cobol-ts-mode-indent-offset)

     ;; Common statements
     ((parent-is "accept_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "add_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "call_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "compute_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "delete_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "display_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "divide_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "initialize_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "inspect_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "move_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "multiply_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "read_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "rewrite_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "string_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "subtract_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "unstring_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "write_statement") parent-bol cobol-ts-mode-indent-offset)

     ;; I/O and control flow
     ((parent-is "open_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "close_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "goto_statement") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "merge_statement") parent-bol cobol-ts-mode-indent-offset)

     ;; Generic statement handling - catch any statement nodes we might have missed
     ((node-is "statement") parent-bol 0)
     ((parent-is "statement") parent-bol cobol-ts-mode-indent-offset)

     ;; Condition and expression indentation
     ((parent-is "condition") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "expression") parent-bol cobol-ts-mode-indent-offset)
     ((parent-is "arithmetic_expression") parent-bol cobol-ts-mode-indent-offset)

     ;; Default: no additional indentation
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `cobol-ts-mode' in free format.")

(defcustom cobol-ts-mode-area-a-column 7
  "Column number for Area A in fixed format COBOL (0-indexed).
Area A is where divisions, sections, and paragraphs start.
Traditional COBOL uses column 7 (8th column, after the indicator area)."
  :type 'integer
  :safe 'integerp
  :group 'cobol-ts)

(defvar cobol-ts-mode--indent-rules-fixed-format
  `((cobol
     ;; Fixed format: respects traditional COBOL column areas
     ;; Area A (columns 8-11): Divisions, sections, paragraphs
     ;; Area B (columns 12-72): Statements

     ;; Top-level structure
     ((node-is "^program_definition$") column-0 0)

     ;; Divisions - start at Area A (column 7, 0-indexed)
     ((node-is "identification_division") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "environment_division") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "data_division") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "procedure_division") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "function_division") column-0 ,cobol-ts-mode-area-a-column)

     ;; Sections - also start at Area A
     ((node-is "configuration_section") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "input_output_section") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "file_section") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "working_storage_section") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "linkage_section") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "local_storage_section") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "screen_section") column-0 ,cobol-ts-mode-area-a-column)
     ((node-is "report_section") column-0 ,cobol-ts-mode-area-a-column)

     ;; Section headers in procedure division - also Area A
     ((node-is "section_header") column-0 ,cobol-ts-mode-area-a-column)

     ;; Paragraph headers - also Area A
     ((node-is "paragraph_header") column-0 ,cobol-ts-mode-area-a-column)

     ;; Everything else goes to Area B (column 11, which is Area A + 4)
     ;; Statements under sections
     ((parent-is "identification_division") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "environment_division") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "data_division") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "procedure_division") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "function_division") column-0 ,(+ cobol-ts-mode-area-a-column 4))

     ;; Configuration and I/O sections content - Area B
     ((parent-is "configuration_section") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "input_output_section") column-0 ,(+ cobol-ts-mode-area-a-column 4))

     ;; Data description sections content - Area B
     ((parent-is "working_storage_section") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "file_section") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "linkage_section") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "local_storage_section") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "screen_section") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "report_section") column-0 ,(+ cobol-ts-mode-area-a-column 4))

     ;; Paragraphs in configuration section - Area B
     ((parent-is "source_computer_paragraph") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "object_computer_paragraph") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "special_names_paragraph") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "repository_paragraph") column-0 ,(+ cobol-ts-mode-area-a-column 4))

     ;; Data and file descriptions - Area B
     ((parent-is "data_description") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "file_description") column-0 ,(+ cobol-ts-mode-area-a-column 4))

     ;; Statements under section header - Area B
     ((parent-is "section_header") column-0 ,(+ cobol-ts-mode-area-a-column 4))

     ;; Statements under paragraph - Area B
     ((parent-is "paragraph_header") column-0 ,(+ cobol-ts-mode-area-a-column 4))

     ;; Perform statements and children - Area B with additional indent
     ((parent-is "perform_statement_call_proc") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "perform_statement_loop") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "perform_procedure") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "perform_option") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "perform_varying") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "perform_test") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; If-then-else structures - Area B with additional indent
     ((parent-is "if_header") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "else_if_header") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((node-is "else_if_header") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((node-is "else_header") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "else_header") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; Evaluate (COBOL's switch/case) - Area B
     ((parent-is "evaluate_header") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "evaluate_subject") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((node-is "when") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "when") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((node-is "when_other") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "when_other") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; Search statement - Area B
     ((parent-is "search_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; Exception handlers for I/O operations - Area B
     ((node-is "at_end") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "at_end") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((node-is "not_at_end") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "not_at_end") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((node-is "invalid_key") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "invalid_key") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((node-is "not_invalid_key") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "not_invalid_key") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((node-is "eop") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "eop") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((node-is "not_eop") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "not_eop") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; Common statements - Area B with additional indent for nested content
     ((parent-is "accept_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "add_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "call_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "compute_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "delete_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "display_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "divide_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "initialize_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "inspect_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "move_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "multiply_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "read_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "rewrite_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "string_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "subtract_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "unstring_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "write_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; I/O and control flow - Area B
     ((parent-is "open_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "close_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "goto_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "merge_statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; Generic statement handling - Area B
     ((node-is "statement") column-0 ,(+ cobol-ts-mode-area-a-column 4))
     ((parent-is "statement") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; Condition and expression indentation - Area B with additional indent
     ((parent-is "condition") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "expression") column-0 ,(+ cobol-ts-mode-area-a-column 8))
     ((parent-is "arithmetic_expression") column-0 ,(+ cobol-ts-mode-area-a-column 8))

     ;; Default: Area B
     (no-node column-0 ,(+ cobol-ts-mode-area-a-column 4))))
  "Tree-sitter indent rules for `cobol-ts-mode' in fixed format.")

(defvar cobol-ts-mode--keywords
  '("ACCEPT" "ACCESS" "ADD" "ADDRESS" "ADVANCING" "AFTER" "ALL" "ALLOCATE"
    "ALPHABET" "ALPHABETIC" "ALPHABETIC-LOWER" "ALPHABETIC-UPPER"
    "ALPHANUMERIC" "ALPHANUMERIC-EDITED" "ALSO" "ALTER" "ALTERNATE" "AND"
    "ANY" "ARE" "AREA" "AREAS" "ASCENDING" "ASSIGN" "AT" "AUTO" "AUTOMATIC"
    "BACKGROUND-COLOR" "BASED" "BEFORE" "BELL" "BINARY" "BLANK" "BLINK"
    "BLOCK" "BOTTOM" "BY" "CALL" "CANCEL" "CHAINING" "CHARACTER" "CHARACTERS"
    "CLASS" "CLOSE" "CODE" "COLLATING" "COLUMN" "COLUMNS" "COMMA" "COMMIT"
    "COMMON" "COMP" "COMPUTE" "CONFIGURATION" "CONSTANT" "CONTAINS" "CONTENT"
    "CONTINUE" "CONTROL" "CONVERTING" "COPY" "CORRESPONDING" "COUNT" "CRT"
    "CURRENCY" "CURSOR" "CYCLE" "DATA" "DATE" "DAY" "DE" "DEBUGGING"
    "DECIMAL-POINT" "DECLARATIVES" "DEFAULT" "DELETE" "DELIMITED" "DELIMITER"
    "DEPENDING" "DESCENDING" "DESTINATION" "DETAIL" "DISABLE" "DISK" "DISPLAY"
    "DIVIDE" "DIVISION" "DOWN" "DUPLICATES" "DYNAMIC" "EBCDIC" "ELSE" "EMI"
    "ENABLE" "END" "END-ACCEPT" "END-ADD" "END-CALL" "END-COMPUTE" "END-DELETE"
    "END-DISPLAY" "END-DIVIDE" "END-EVALUATE" "END-IF" "END-MULTIPLY"
    "END-OF-PAGE" "END-PERFORM" "END-READ" "END-RETURN" "END-REWRITE"
    "END-SEARCH" "END-START" "END-STRING" "END-SUBTRACT" "END-UNSTRING"
    "END-WRITE" "ENTRY" "ENVIRONMENT" "EOP" "EQUAL" "ERROR" "ESCAPE" "ESI"
    "EVALUATE" "EVERY" "EXCEPTION" "EXCLUSIVE" "EXIT" "EXTEND" "EXTERNAL"
    "FALSE" "FD" "FILE" "FILE-CONTROL" "FILLER" "FINAL" "FIRST" "FOOTING"
    "FOR" "FOREGROUND-COLOR" "FORMAT" "FREE" "FROM" "FUNCTION" "GENERATE"
    "GIVING" "GLOBAL" "GO" "GOBACK" "GREATER" "GROUP" "HEADING" "HIGHLIGHT"
    "HIGH-VALUE" "HIGH-VALUES" "I-O" "I-O-CONTROL" "IDENTIFICATION" "IF"
    "IN" "INDEX" "INDEXED" "INDICATE" "INITIAL" "INITIALIZE" "INITIATE"
    "INPUT" "INPUT-OUTPUT" "INSPECT" "INSTALLATION" "INTO" "INVALID" "IS"
    "JUST" "JUSTIFIED" "KEY" "LABEL" "LAST" "LEADING" "LEFT" "LENGTH" "LESS"
    "LIMIT" "LIMITS" "LINAGE" "LINAGE-COUNTER" "LINE" "LINES" "LINKAGE"
    "LOCAL-STORAGE" "LOCK" "LOWLIGHT" "LOW-VALUE" "LOW-VALUES" "MEMORY"
    "MERGE" "MESSAGE" "MODE" "MODULES" "MOVE" "MULTIPLE" "MULTIPLY" "NATIVE"
    "NEGATIVE" "NEXT" "NO" "NOT" "NULL" "NULLS" "NUMBER" "NUMERIC"
    "NUMERIC-EDITED" "OBJECT-COMPUTER" "OCCURS" "OF" "OFF" "OMITTED" "ON"
    "OPEN" "OPTIONAL" "OR" "ORDER" "ORGANIZATION" "OTHER" "OUTPUT" "OVERFLOW"
    "PACKED-DECIMAL" "PADDING" "PAGE" "PAGE-COUNTER" "PARAGRAPH" "PERFORM"
    "PIC" "PICTURE" "PLUS" "POINTER" "POSITION" "POSITIVE" "PRINTING"
    "PROCEDURE" "PROCEDURES" "PROCEED" "PROGRAM" "PROGRAM-ID" "PROMPT"
    "QUOTE" "QUOTES" "RANDOM" "RD" "READ" "RECEIVE" "RECORD" "RECORDING"
    "RECORDS" "RECURSIVE" "REDEFINES" "REEL" "REFERENCE" "REFERENCES"
    "RELATIVE" "RELEASE" "RELOAD" "REMAINDER" "REMOVAL" "RENAMES" "REPLACE"
    "REPLACING" "REPORT" "REPORTING" "REPORTS" "REPOSITORY" "RERUN" "RESERVE"
    "RESET" "RETURN" "RETURN-CODE" "REVERSED" "REWIND" "REWRITE" "RIGHT"
    "ROLLBACK" "ROUNDED" "RUN" "SAME" "SCREEN" "SD" "SEARCH" "SECTION"
    "SECURITY" "SEGMENT" "SEGMENT-LIMIT" "SELECT" "SEND" "SENTENCE" "SEPARATE"
    "SEQUENCE" "SEQUENTIAL" "SET" "SHARING" "SIGN" "SIZE" "SORT" "SORT-MERGE"
    "SOURCE" "SOURCE-COMPUTER" "SPACE" "SPACES" "SPECIAL-NAMES" "STANDARD"
    "STANDARD-1" "STANDARD-2" "START" "STATUS" "STOP" "STRING" "SUB-QUEUE-1"
    "SUB-QUEUE-2" "SUB-QUEUE-3" "SUBTRACT" "SUM" "SUPPRESS" "SYMBOLIC"
    "SYNC" "SYNCHRONIZED" "TABLE" "TALLYING" "TAPE" "TERMINAL" "TERMINATE"
    "TEST" "TEXT" "THAN" "THEN" "THROUGH" "THRU" "TIME" "TIMES" "TO" "TOP"
    "TRAILING" "TRUE" "TYPE" "UNDERLINE" "UNIT" "UNLOCK" "UNSTRING" "UNTIL"
    "UP" "UPDATE" "UPON" "USAGE" "USE" "USING" "VALUE" "VALUES" "VARYING"
    "WHEN" "WHEN-OTHER" "WITH" "WORDS" "WORKING-STORAGE" "WRITE")
  "COBOL keywords for tree-sitter font-locking.")

(defvar cobol-ts-mode--constants
  '("ZERO" "ZEROS" "ZEROES" "SPACE" "SPACES" "QUOTE" "QUOTES"
    "HIGH-VALUE" "HIGH-VALUES" "LOW-VALUE" "LOW-VALUES"
    "NULL" "NULLS" "TRUE" "FALSE")
  "COBOL constants for tree-sitter font-locking.")

(defvar cobol-ts-mode--operators
  '("+" "-" "*" "/" "**" "=" ">" "<" ">=" "<=" "NOT" "AND" "OR")
  "COBOL operators for tree-sitter font-locking.")

(defvar cobol-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'cobol
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'cobol
   :feature 'string
   '([(string) (h_string) (x_string) (n_string)] @font-lock-string-face)

   :language 'cobol
   :feature 'number
   '([(integer) (decimal) (number)] @font-lock-number-face)

   :language 'cobol
   :feature 'constant
   '(;; Note: Most COBOL constants appear as regular identifiers in the parse tree
     ;; We highlight specific constant-like nodes that do appear
     (SPACE) @font-lock-constant-face)

   :language 'cobol
   :feature 'keyword
   '(;; Keywords that commonly appear in parse trees
     (COMP) @font-lock-keyword-face
     (FOREVER) @font-lock-keyword-face
     (END_ACCEPT) @font-lock-keyword-face
     (END_IF) @font-lock-keyword-face
     (END_PERFORM) @font-lock-keyword-face)

   :language 'cobol
   :feature 'operator
   `(["+" "-" "*" "/" "**" "=" ">" "<" ">=" "<="] @font-lock-operator-face)

   :language 'cobol
   :feature 'delimiter
   '(["." "," ";" ":"] @font-lock-delimiter-face)

   :language 'cobol
   :feature 'bracket
   '(["(" ")" "[" "]"] @font-lock-bracket-face)

   :language 'cobol
   :feature 'type
   '(;; Picture clauses
     (picture_clause) @font-lock-type-face
     (picture_x) @font-lock-type-face
     (picture_n) @font-lock-type-face
     (picture_9) @font-lock-type-face
     (picture_a) @font-lock-type-face
     (picture_edit) @font-lock-type-face

     ;; Data types
     [(BINARY) (BINARY_CHAR) (BINARY_C_LONG) (BINARY_DOUBLE) (BINARY_LONG) (BINARY_SHORT)
      (COMP) (COMPUTATIONAL) (COMP_1) (COMP_2) (COMP_3) (COMP_4) (COMP_5) (COMP_X)
      (DISPLAY) (INDEX) (PACKED_DECIMAL) (POINTER)] @font-lock-type-face)

   :language 'cobol
   :feature 'level-number
   '((level_number) @font-lock-number-face)

   :language 'cobol
   :feature 'statement
   '(;; All statement types that exist in the grammar
     [(accept_statement) (add_statement) (allocate_statement) (alter_statement)
      (call_statement) (cancel_statement) (close_statement) (compute_statement)
      (continue_statement) (copy_statement) (delete_statement) (display_statement)
      (divide_statement) (exit_statement) (goback_statement) (goto_statement)
      (initialize_statement) (inspect_statement) (merge_statement) (move_statement)
      (multiply_statement) (next_sentence_statement) (open_statement)
      (perform_statement_loop) (perform_statement_call_proc) (read_statement)
      (release_statement) (return_statement) (rewrite_statement) (search_statement)
      (select_statement) (set_statement) (sort_statement) (start_statement)
      (stop_statement) (string_statement) (subtract_statement) (unstring_statement)
      (use_statement) (write_statement)] @font-lock-builtin-face)

   :language 'cobol
   :feature 'division
   '(;; Divisions and sections
     (identification_division) @font-lock-preprocessor-face
     (environment_division) @font-lock-preprocessor-face
     (data_division) @font-lock-preprocessor-face
     (procedure_division) @font-lock-preprocessor-face
     (configuration_section) @font-lock-preprocessor-face
     (input_output_section) @font-lock-preprocessor-face
     (file_section) @font-lock-preprocessor-face
     (working_storage_section) @font-lock-preprocessor-face
     (linkage_section) @font-lock-preprocessor-face
     (local_storage_section) @font-lock-preprocessor-face
     (screen_section) @font-lock-preprocessor-face
     (report_section) @font-lock-preprocessor-face
     (section_header) @font-lock-preprocessor-face
     (paragraph_header) @font-lock-preprocessor-face)

   :language 'cobol
   :feature 'program-name
   '((program_name) @font-lock-constant-face)

   :language 'cobol
   :feature 'variable
   '(;; Entry names (data items)
     (entry_name) @font-lock-variable-name-face

     ;; Qualified data names
     (qualified_word) @font-lock-variable-use-face)

   :language 'cobol
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `cobol-ts-mode'.")

;;;###autoload
(define-derived-mode cobol-ts-mode prog-mode "COBOL"
  "Major mode for editing COBOL files with tree-sitter support.

\\{cobol-ts-mode-map}"
  :group 'cobol-ts
  :syntax-table cobol-ts-mode--syntax-table

  (when (treesit-ready-p 'cobol)
    (treesit-parser-create 'cobol)

    ;; Comments
    (setq-local comment-start "*> ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "\\(?:\\*>\\|^\\*\\)[ \t]*")

    ;; Font-lock
    (setq-local treesit-font-lock-settings cobol-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword constant number type)
                  (statement division program-name level-number variable)
                  (operator delimiter bracket error)))

    ;; Indentation - use fixed format rules
    ;; Note: The tree-sitter grammar only supports fixed-format COBOL
    (setq-local treesit-simple-indent-rules
                cobol-ts-mode--indent-rules-fixed-format)

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                (rx (or "paragraph_header" "section_header"
                        "program_definition" "function_division")))

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                `(("Division" "\\`.*_division\\'" nil nil)
                  ("Section" "\\`.*_section\\'" nil nil)
                  ("Paragraph" "\\`paragraph_header\\'" nil nil)
                  ("Program" "\\`program_definition\\'" nil nil)))

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cob\\'" . cobol-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cbl\\'" . cobol-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cobol\\'" . cobol-ts-mode))

(provide 'cobol-ts-mode)

;;; cobol-ts-mode.el ends here

