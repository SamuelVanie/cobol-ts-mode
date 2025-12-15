# cobol-ts-mode

A tree-sitter major mode for COBOL in Emacs with full syntax highlighting, indentation, and navigation support.

## Features

- **Tree-sitter Support**: Full AST-based syntax highlighting and indentation for fixed-format COBOL
- **Free Format Support**: Regex-based keyword highlighting for free-format COBOL (>>SOURCE FORMAT FREE)
- **Syntax Highlighting**: Comprehensive keyword, string, number, and comment highlighting
- **Smart Indentation**: Automatic indentation for divisions, sections, paragraphs, and statements
- **Navigation**: Jump between divisions, sections, and paragraphs
- **Multiple File Extensions**: Supports `.cob`, `.cbl`, and `.cpy` files

## Requirements

- Emacs 29 or later (tree-sitter enabled)
- Tree-sitter COBOL grammar (installed via the included command)

## Installation

1. Install the package

(use-package cobol-ts-mode
  :vc (:url "https://github.com/SamuelVanie/cobol-ts-mode"
       :branch "main"))

or install with straight

```elisp
(use-package cobol-ts-mode
    :straight (cobol-ts-mode :type git :host github :repo "SamuelVanie/cobol-ts-mode" :files ("*.el")))
```

2. Install the tree-sitter COBOL grammar:

```
M-x cobol-ts-install-grammar
```

This will download and compile the grammar from [Yutaro Sakamoto's tree-sitter-cobol implementation](https://github.com/yutaro-sakamoto/tree-sitter-cobol).

## Usage

The mode automatically activates for files with `.cob`, `.cbl`, or `.cpy` extensions.

### Fixed Format COBOL

For fixed-format COBOL files, the mode uses tree-sitter for full syntax analysis:
- Full syntax highlighting with context-aware fontification
- Automatic indentation based on COBOL structure
- Defun navigation (jump between divisions, sections, and paragraphs)

### Free Format COBOL

For free-format COBOL (files containing `>>SOURCE FORMAT FREE`), the mode uses regex-based keyword highlighting:
- Basic keyword highlighting
- Simple indentation rules
- All procedural keywords highlighted

## Customization

### Indentation

Control the indentation offset (default 4 spaces):

```elisp
(setq cobol-ts-mode-indent-offset 4)
```

### Comments

The mode recognizes:
- Fixed format: `*` in column 7 for comments
- Free format: `*>` for comments

## Supported COBOL Constructs

### Keywords Highlighted

- **Divisions**: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
- **Sections**: CONFIGURATION, INPUT-OUTPUT, FILE, WORKING-STORAGE, LINKAGE, LOCAL-STORAGE
- **Statements**: PERFORM, IF/ELSE, EVALUATE, MOVE, DISPLAY, READ, WRITE, OPEN, CLOSE, etc.
- **Clauses**: PICTURE/PIC, VALUE, USAGE, OCCURS, REDEFINES

### Data Types

- COMP, COMP-3, BINARY, PACKED-DECIMAL, DISPLAY, INDEX

## Implementation Notes

The mode uses different strategies for different COBOL formats:

- **Fixed Format**: Tree-sitter parser with custom fontification callbacks for accurate keyword highlighting (keywords may span multiple nodes)
- **Free Format**: Regex-based font-lock rules (tree-sitter parser less reliable for free-format syntax)

The tree-sitter grammar override configuration handles the COBOL grammar's uppercase export symbol (`tree_sitter_COBOL` vs. `tree_sitter_cobol`).

## License

Do whatever you want
