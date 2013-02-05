.. _Roadmap:

Roadmap
=======

Introduction
------------
This document captures the status of development and future roadmap for the Swift language, compiler, and related tools. It is a perpetual work in progress, meant to give a rough sense of where we are and where we are going.

Language Features
-----------------

+--------------------------+--------+--------------+----------+-------------+
|Feature                   |Design  |Parse/AST/Sema|IRgen     |Dependencies |
+==========================+========+==============+==========+=============+
|*Pattern Matching*        |   X    |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- oneofs                  |   X    |     X        |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Switch/Case             |   X    |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Optional<T>             |   X    |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Match expressions       |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Modules*                 |        |              |          |Name lookup  |
+--------------------------+--------+--------------+----------+-------------+
|- Namespaces/packages     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Binary modules          |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Inline functions        |        |              |          |SIL          |
+--------------------------+--------+--------------+----------+-------------+
|- Import Clang modules    |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Multiple-TU modules     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Error Handling /         |        |              |          |SIL          |
|Exceptions*               |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- NSError interop         |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- POSIX interop           |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- C++ exceptions interop  |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Memory Management*       |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Weak pointers           |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Closures and cycles     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- nocapture               |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Fragility Model*         |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Availability info       |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- API evolution           |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Conditional Compilation* |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Objective-C              |        |              |          |             |
|Interoperability*         |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Extensions as categories|        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Protocols               |   X    |     X        |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Properties              |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- CoreData                |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- id and Class types      |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Constructors vs init    |   X    |     X        |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- API portability diags   |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Submodules support      |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Inline C functions      |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- CF interoperability     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Objective-C Extensions*  |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Typed collections       |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- API portability diags   |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Loading Swift modules   |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Extending Swift types   |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Generics*                |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Default implementations |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Extending protocols     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Tuples                  |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Non-type parameters     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- <> in expressions       |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Strong Typedefs*         |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Actors*                  |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Key-Value Observing*     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Objective-C interop     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Type State*              |        |              |          |SIL          |
+--------------------------+--------+--------------+----------+-------------+
|*Non-Null Pointers*       |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Reflection/Introspection*|        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Regular Expressions*     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Constant Expressions*    |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Immutability Model*      |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Container Literals*      |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Array literals          |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- Dictionary literals     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Fixed-size Arrays*       |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Overload Resolution*     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|*Performance*             |        |              |          |SIL          |
+--------------------------+--------+--------------+----------+-------------+
|- Inlining                |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+
|- LTO                     |        |              |          |             |
+--------------------------+--------+--------------+----------+-------------+

Compiler Engineering Tasks
--------------------------

* SIL

  * IRgen via SIL
  * Data-flow diagnostics (guaranteed initialization, guaranteed return, etc.)
  * SIL-based optimizations

* Source Fidelity

  * "Perfect" AST pretty-printing
  * Source location information for every token written
  * "Implicit" annotations for AST nodes not written

* Generics

  * Reimplement “conforms to protocol” to eliminate extraneous typing
  * Replace all informal protocols (e.g., for literals) with formal protocols
  * Parsing <> in an expression context

* Debug Info

  * Basic line info
  * Basic variable/type information (until debugger work proceeds in earnest)

* Name Lookup

  * Rewrite type binding to cope with nesting, recursion, etc.
  * Implement name-hiding rules for modules, extensions
  * Rewrite with actual hash tables

* Type Checker

  * Dependencies: Modules (partial, to alleviate performance problems)
  * Diagnostics
  * Switch to new type checker, remove old type checking code
  * Performance
  * Overload resolution

* Driver

  * Build executables
  * Build shared libraries

* Build system

* Documentation

  * Compiler
  * REPL
  * Introduction
  * Language manual
  * Language specification

* Diagnostics

  * Fix-Its
  * Warning flags + suppression mechanism
  * Vend to Xcode
  * Diagnostic categories

* IRgen

  * Level-of-abstraction differences for generics
  * Using Clang types, calling convention code for interoperability
  * Writeback
  * Nested generics

Tools and Integration
---------------------

* Indexer

  * Coordinate with existing indexer for cross-language indexing
  * Go-To-Definition

* Source Editor

  * Tokenization
  * Code folding
  * Syntax highlighting
  * Code formatting
  * Code Completion

* Documentation Parsing

  * Code completion
  * QuickHelp
  * DevPubs
  * Showing API in Xcode without showing source

* Interface Builder

  * IBOutlet/IBOutletCollection attributes
  * Connect to Swift sources

* REPL

  * Code completion
  * Integration with source editor/Xcode
  * Syntax coloring

* Debugger

  * Integrate with REPL
  * Cross-language debugging

* API Evolution Checking Tool

* Static Analyzer

* Instruments

* Packaging and distribution tool

  * Command line
  * Xcode integration

* SDK Issues

  * Explicit properties
  * Explicit conformance to protocol
  * Consistent NS_ENUM/NS_OPTION use
  * Consitent ARC annotations  
