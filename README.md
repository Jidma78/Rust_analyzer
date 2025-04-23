# Rustine Analyzer
Lexical, syntactic, and semantic analyzer for a Rust subset, developed as part of a assembly project.

## Description
This project implements a full pipeline for parsing a simplified Rust-like language called Rustine. It includes:

- A lexical and syntactic analyzer built with ocamllex and ocamlyacc
- An AST (Abstract Syntax Tree) generator
- Semantic checks:
    - Scope and variable declaration validation
    - Mutability checks for assignments
    - Type checking on expressions and control structures
    - Extraction and printing of numeric and boolean constants

## Prerequisites
  - OCaml (≥ 4.08 recommended)
  - ocamllex and ocamlyacc (or their equivalents like menhir)
  - make

## Usage
1. Clone the repository

2. Compile with:

    ```shell
    make
    ```
      
3. Run the analyzer with a .rs file (Rustine syntax):

    ```shell
    ./rustine tests/example.rs
    ```

The program will:
  - Print the constants
  - Run scope, affectation, and type checks
  - Report errors if semantic rules are violated

Test files are provided in the tests/ folder (both valid and invalid).

## Next Improvements
  - Improve error messages and debug feedback
  - Extend grammar coverage and add deeper semantic inference
  - Modularize codebase for better maintainability

