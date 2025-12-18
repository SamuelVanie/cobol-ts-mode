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
(eval-when-compile (require 'rx))

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

(defun cobol-ts-mode--is-top-level-data-p (node _ _ &rest _)
  "Return t if NODE is a data entry starting with 01, 77, FD, or SD."
  (let ((text (treesit-node-text node)))
    (string-match-p (rx bol (0+ space) 
                        (or "01" "77" "FD" "SD")) 
                    text)))

(defun cobol-ts-mode--is-sub-level-data-p (node _ _ &rest _)
  "Return t if NODE is a data entry that is NOT 01, 77, FD, or SD."
  (let ((text (treesit-node-text node)))
    (and (string-match-p (rx bol (0+ space) digit) text) ;; Must be a number
         (not (string-match-p (rx bol (0+ space) 
                                  (or "01" "77" "FD" "SD")) 
                              text)))))


(defvar cobol-ts-mode--handler-nodes
  (rx (or "on_exception"      "not_on_exception"
          "on_size_error"     "not_on_size_error"
          "on_overflow"       "not_on_overflow"
          "at_end"            "not_at_end"
          "eop"               "not_eop"
          "invalid_key"       "not_invalid_key"
          "evaluate_header"   
          "when"              "when_other"
          "perform_statement_loop"
          "if_header"         "else_if_header"    "else_header"))
  "Regex matching COBOL handler nodes that should align with their parent.")


(defvar cobol-ts-mode--indent-rules-fixed-format
  `((cobol
     ;; Fixed format: respects traditional COBOL column areas
     ;; Area A (columns 8-11): Divisions, sections, paragraphs
     ;; Area B (columns 12-72): Statements

     ;; Top-level structure
     ((node-is "program_definition") column-0 0)
     ((node-is "function_definition") column-0 0)

     ;; ====================================================================
     ;; 1. ZONE A: ANCHORS (Headers)
     ;; ====================================================================
     ;; Match explicit DIVISION/SECTION headers by NODE type
     ((node-is ,(rx (or (seq "_division" eos) 
                        (seq "_section" eos)))) 
      column-0 cobol-ts-mode-area-a-column)
     
     ;; Match Paragraphs (PROGRAM-ID, FILE-CONTROL, Main Paragraphs) by NODE type
     ((node-is ,(rx "_paragraph" eos)) 
      column-0 cobol-ts-mode-area-a-column)

     
     ;; ====================================================================
     ;; DATA DIVISION SPECIFICS
     ;; ====================================================================

     ;; 1. TOP LEVEL ITEMS (01, 77, FD, SD)
     ;; These go in Area A (same as the Section Header).
     ;; We anchor them to the parent (the Section) with 0 offset.
     ((and (node-is "data_description") 
           cobol-ts-mode--is-top-level-data-p)
      parent-bol 0)
     
     ;; Also handle File Descriptions (FD/SD) if the grammar names them differently
     ((node-is "file_description_entry")
      parent-bol 0)

     ;; 2. SUB-LEVEL ITEMS (05, 10, 88, etc.)
     ;; These go in Area B.
     ;; Anchor to the Parent (Section) + Indent Offset.
     ((and (node-is "data_description") 
           cobol-ts-mode--is-sub-level-data-p)
      parent-bol cobol-ts-mode-indent-offset)

     ;; 3. CLAUSES ON NEW LINES (PIC, VALUE, OCCURS)
     ;; If you split a definition like:
     ;; 05 MY-VAR
     ;;    PIC X.
     ;; The 'PIC X' is a child of 'data_description'.
     ((parent-is "data_description") parent-bol cobol-ts-mode-indent-offset)

     ;; 4. FILE DESCRIPTION CHILDREN
     ;; Inside an FD, usually we indent the BLOCK CONTAINS, LABEL RECORD, etc.
     ((parent-is "file_description_entry")
      parent-bol cobol-ts-mode-indent-offset)



     ;; ====================================================================
     ;; GENERIC BLOCK CONTENT (Zone B)
     ;; ====================================================================

     ;; 1. SECTION CONTENT
     ;; If we are inside a Section, indent the content.
     ;; BUT WAIT! Paragraphs are children of Sections too. 
     ;; Because we have a higher-priority rule ((node-is "_paragraph") parent-bol 0),
     ;; the paragraphs will stay left. Everything ELSE (variables, etc.) will indent.
     ((parent-is ,(rx "_section" eos)) parent-bol cobol-ts-mode-indent-offset)

     ;; 2. PARAGRAPH CONTENT (The most common rule)
     ;; This handles:
     ;; - Procedure Division: Statements inside paragraphs.
     ;; - Environment Division: Clauses inside SOURCE-COMPUTER, FILE-CONTROL.
     ;; - Identification Division: Text inside AUTHOR, PROGRAM-ID.
     ((parent-is ,(rx "_paragraph" eos)) parent-bol cobol-ts-mode-indent-offset)

     ;; 3. DIVISION CONTENT (Direct children)
     ;; Usually Divisions contain Sections (Zone A). 
     ;; But sometimes they contain direct code/paragraphs.
     ((parent-is ,(rx "_division" eos)) parent-bol cobol-ts-mode-indent-offset)
     
     ;; ====================================================================
     ;; PROCEDURE DIVISION: ALIGNMENT (The "Un-indenters")
     ;; ====================================================================
     ;; These nodes appear inside a statement but must align with the start.
     ;; e.g.
     ;; READ ...
     ;;    AT END     <-- Matches here (Aligns to READ)
     ;;       DISPLAY...
     
     ;; 1. Terminators (END-IF, END-READ)
     ;;
     ((node-is ,(rx (or (seq "end_" (0+ any)) 
                        (seq "END_" (0+ any))))) 
      parent-bol 0)

     ;; 2. Handlers & Headers (ELSE, WHEN, AT END, ON SIZE ERROR)
     ((node-is ,cobol-ts-mode--handler-nodes) parent-bol 0)

     ;; ====================================================================
     ;; PROCEDURE DIVISION: NESTING (The "Indenters")
     ;; ====================================================================

     ;; 1. Code inside Handlers
     ;; If the grammar nests statements inside "at_end" or "else_header" nodes,
     ;; this rule indents the content.
     ((parent-is ,cobol-ts-mode--handler-nodes) parent-bol cobol-ts-mode-indent-offset)

     ;; 2. Code inside Main Statements
     ;; Standard indentation for statements inside IF, PERFORM, READ, etc.
     ;; We assume the main container ends in "_statement" (e.g. if_statement, read_statement).
     ((parent-is ,(rx "_statement" eos)) parent-bol cobol-ts-mode-indent-offset)

     
     
     ;; Default: Area B
     (no-node column-0 ,(+ cobol-ts-mode-area-a-column cobol-ts-mode-indent-offset))))
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
  '(
    :language cobol
    :feature comment
    ((comment) @font-lock-comment-face)

    :language cobol
    :feature string
    ([(string) (h_string) (x_string) (n_string)] @font-lock-string-face)

    :language cobol
    :feature number
    ([(integer) (decimal) (number)] @font-lock-constant-face)

    :language cobol
    :feature operator
    (["+" "-" "*" "/" "**" "=" ">" "<" ">=" "<="] @font-lock-constant-face)

    :language cobol
    :feature delimiter
    (["." "," ";" ":"] @font-lock-constant-face)

    :language cobol
    :feature type
    ((picture_clause) @font-lock-type-face
     (picture_x) @font-lock-type-face
     (picture_n) @font-lock-type-face
     (picture_9) @font-lock-type-face
     (picture_a) @font-lock-type-face
     (picture_edit) @font-lock-type-face)

    :language cobol
    :feature division
    :override t
    ((identification_division) @font-lock-preprocessor-face)


    :language cobol
    :feature keyword
    ;; Scope terminators - important for readability
    ([(END_IF) (END_PERFORM) (END_EVALUATE) (END_READ)
      (END_ACCEPT) (END_ADD) (END_CALL) (END_COMPUTE)
      (END_DELETE) (END_DISPLAY) (END_DIVIDE) (END_MULTIPLY)
      (END_RETURN) (END_REWRITE) (END_SEARCH) (END_START)
      (END_STRING) (END_SUBTRACT) (END_UNSTRING) (END_WRITE)] @font-lock-keyword-face)


    :language cobol
    :feature control-flow
    ;; Conditional structures
    ([(if_header) (else_if_header) (else_header)
      (evaluate_header) (when) (when_other)] @font-lock-keyword-face)

    :language cobol
    :feature control-flow
    ;; Loop structures
    ([(perform_statement_loop) (perform_statement_call_proc)
      (perform_varying) (perform_test)] @font-lock-keyword-face)

    :language cobol
    :feature control-flow
    ;; Exception handlers
    ([(on_exception) (not_on_exception)
      (on_size_error) (not_on_size_error)
      (on_overflow) (not_on_overflow)
      (at_end) (not_at_end)
      (eop) (not_eop)
      (invalid_key) (not_invalid_key)] @font-lock-warning-face)

    :language cobol
    :feature control-flow
    ;; Control transfer
    ([(goto_statement) (goback_statement) (exit_statement)
      (stop_statement) (next_sentence_statement)] @font-lock-keyword-face)


    :language cobol
    :feature constant
    ;; Figurative constants
    ([(SPACE) (ZERO) (ZEROS)
      (HIGH_VALUE) (LOW_VALUE)
      (QUOTE) (TOK_NULL)
      (TRUE) (FALSE)] @font-lock-constant-face)

    :language cobol
    :feature constant
    ;; Special identifiers
    ([(POSITIVE) (NEGATIVE) (ALPHABETIC) (ALPHABETIC_LOWER) (ALPHABETIC_UPPER)
      (ALPHANUMERIC) (ALPHANUMERIC_EDITED) (NUMERIC) (NUMERIC_EDITED)
      (OMITTED)] @font-lock-constant-face)

    :language cobol
    :feature constant
    ;; Reserved words with constant-like meaning
    ([(FILLER)] @font-lock-constant-face)

    :language cobol
    :feature variable
    ;; Special program identifiers
    ((program_name) @font-lock-constant-face)

    :language cobol
    :feature function
    ;; Function names as constants (built-in function keywords)
    ([(CURRENT_DATE_FUNC) (LOCALE_DT_FUNC) (LOWER_CASE_FUNC)
      (UPPER_CASE_FUNC) (REVERSE_FUNC) (TRIM_FUNCTION)
      (CONCATENATE_FUNC) (NUMVALC_FUNC) (SUBSTITUTE_FUNC)
      (SUBSTITUTE_CASE_FUNC) (WHEN_COMPILED_FUNC)] @font-lock-function-name-face)

    :language cobol
    :feature definition
    ;; Level numbers in data descriptions - make them stand out
    ((data_description (level_number) @font-lock-preprocessor-face))

    :language cobol  
    :feature definition
    ;; Special level 88 for condition names
    ((level_number_88) @font-lock-preprocessor-face)
    
    :language cobol
    :feature error
    ((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `cobol-ts-mode'.")

;;;###autoload
(define-derived-mode cobol-ts-mode prog-mode "COBOL[TS]"
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

    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (string constant number keyword type)
		  (builtin division section paragraph control-flow function)
                  (variable operator delimiter bracket error)))

    ;; Font-lock
    (setq-local treesit-font-lock-settings (apply #'treesit-font-lock-rules cobol-ts-mode--font-lock-settings))

    ;; fallback for the different keywords
    (font-lock-add-keywords
     'cobol-ts-mode
     `((,(regexp-opt cobol-ts-mode--keywords 'words) . font-lock-keyword-face)
       (,(regexp-opt cobol-ts-mode--constants 'words) . font-lock-constant-face)
       (,(regexp-opt cobol-ts-mode--operators 'words) . font-lock-builtin-face)))

    
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

