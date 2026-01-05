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

;;;###autoload
(defun cobol-ts-install-grammar ()
  "Install the COBOL grammar for tree-sitter."
  (interactive)
  (add-to-list 'treesit-language-source-alist
               '(cobol .
		       ("https://github.com/SamuelVanie/tree-sitter-cobol" "main" "src")))
  (treesit-install-language-grammar 'cobol))

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

    ;; ;; could be in variable's names
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


(defun cobol-ts--anchor-find-matching-start (node parent _ &rest _)
  "Anchor to the matching opening node for the current NODE.
This handles flat structures where IF, body, and END-IF are siblings.
It counts nested blocks to find the correct start."
  (let ((current-type (treesit-node-type node))
        (opener-type nil)
        (balance 0)
        (iter (treesit-node-prev-sibling node))
        (found-pos nil))
    
    ;; Determine what we are looking for based on the current node
    (cond
     ((member current-type '("END_IF" "else_header"))
      (setq opener-type "if_header"))
     ((member current-type '("END_EVALUATE" "when"))
      (setq opener-type "evaluate_header")))
    
    (if (not opener-type)
        ;; Fallback: if we aren't a closer, just use parent start
        (treesit-node-start parent)
      
      ;; Scan backwards looking for the opener
      (while (and iter (not found-pos))
        (let ((type (treesit-node-type iter)))
          (cond
           ;; If we see a closer (like another END-IF), increment balance (nested block)
           ((equal type current-type)
            (setq balance (1+ balance)))
           
           ;; If we see the opener we want
           ((equal type opener-type)
            (if (= balance 0)
                (setq found-pos (treesit-node-start iter))
              ;; If balance > 0, we just closed a nested block. Decrement.
              (setq balance (1- balance))))))
        (setq iter (treesit-node-prev-sibling iter)))
      
      ;; Return found position or parent start if syntax is broken
      (or found-pos (treesit-node-start parent)))))

(defvar cobol-ts-mode--indent-rules-fixed-format
  `((cobol
     ;; DISCLAIMER, I WORK ON THE PARENT
     ;; IN THIS TREESITTER IMPLEMENTATION THE CURRENT NODE SEEMS
     ;; TO ALWAYS BE NIL EXCEPT FOR DATA_DESCRIPTION

     ;; ==============================================================
     ;; Fixed format: respects traditional COBOL column areas
     ;; Area A (columns 8-11): Divisions, sections, paragraphs
     ;; Area B (columns 12-72): Statements
     ;; ==============================================================

     ;; identification division for sure
     ((parent-is "start") column-0 cobol-ts-mode-area-a-column)
     
     ((n-p-gp nil "program_definition" "start")
      column-0 cobol-ts-mode-area-a-column)
     
     ((parent-is "identification_division") column-0 cobol-ts-mode-area-a-column)

     ;; ====================================================================
     ;; 1. ZONE A: ANCHORS (Headers)
     ;; ====================================================================
     ;; Match explicit DIVISION/SECTION headers by NODE type
     ((node-is ,(rx (seq "_division" eos)))
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
     
     ;; handle file descriptions
     ((node-is "file_description")
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


     ;; 3. data division's children should be aligned with it 
     ((n-p-gp nil ,(rx "data_division" eos) "program_definition")
      column-0 cobol-ts-mode-area-a-column)

     ;; ====================================================================
     ;; GENERIC BLOCK CONTENT (Zone B)
     ;; ====================================================================

     ;; 1. SECTION CONTENT
     ;; If we are inside a Section, indent the content.
     ;; Because we have a higher-priority rule ((node-is "_paragraph") parent-bol 0),
     ;; the paragraphs will stay left. Everything ELSE (variables, etc.) will indent.
     ((parent-is ,(rx "_section" eos)) parent-bol cobol-ts-mode-indent-offset)

     ;; 3. DIVISION CONTENT (Direct children)
     ;; First child should be indented
     ;; But what happens ? Every child seems to be considered the first child
     ;; There's really something wrong with this treesitter implementation
     ;; or is it just me that has skill issues ?
     ;; ((match nil ,(rx "_division" eos) nil 0 0) parent-bol cobol-ts-mode-indent-offset)
     
     ))
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
    ((data_description
      [(picture_clause) @font-lock-type-face
       (picture_x) @font-lock-type-face
       (picture_n) @font-lock-type-face
       (picture_9) @font-lock-type-face
       (picture_a) @font-lock-type-face
       (picture_edit) @font-lock-type-face]))

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
    ((ERROR) @font-lock-warning-face)
    )
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

    ;; Font-lock
    (setq-local treesit-font-lock-settings (apply #'treesit-font-lock-rules cobol-ts-mode--font-lock-settings))
    
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (string constant number keyword type)
		  (builtin division section paragraph control-flow function)
                  (variable operator delimiter bracket error)
		  ))

    ;; fallback for the different keywords
    ;; (font-lock-add-keywords
    ;;  'cobol-ts-mode
    ;;  `((,(regexp-opt cobol-ts-mode--keywords 'words) . font-lock-keyword-face)
    ;;    (,(regexp-opt cobol-ts-mode--constants 'words) . font-lock-constant-face)))

    
    ;; Indentation - use fixed format rules
    ;; Note: The tree-sitter grammar only supports fixed-format COBOL
    (setq-local treesit-simple-indent-rules
                cobol-ts-mode--indent-rules-fixed-format)
    (setq-local indent-tabs-mode nil)
    (setq-local tab-stop-list '(7 11 15))

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

(add-to-list 'auto-mode-alist '("\\.cob\\'" . cobol-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cbl\\'" . cobol-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cobol\\'" . cobol-ts-mode))

(provide 'cobol-ts-mode)

;;; cobol-ts-mode.el ends here

