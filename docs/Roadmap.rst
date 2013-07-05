.. @raise litre.TestsAreMissing
.. _Roadmap:

Roadmap
=======

Introduction
------------
This document captures the status of development and future roadmap for the Swift language, compiler, and related tools. It is a perpetual work in progress, meant to give a rough sense of where we are and where we are going.

Language Features
-----------------

+------------------------------+--------+--------------+----------+-------------+
|Feature                       |Design  |Parse/AST/Sema|IRgen     |Dependencies |
+==============================+========+==============+==========+=============+
|*Control Flow*                |   X    |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- if/while/do/for             |   X    |     X        |    X     |             |
+------------------------------+--------+--------------+----------+-------------+
|- for-each                    |   X    |     X        |    X     |             |
+------------------------------+--------+--------------+----------+-------------+
|- break/continue              |   X    |     X        |    X     |             |
+------------------------------+--------+--------------+----------+-------------+
|- labeled break/continue      |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Pattern Matching*            |   X    |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- oneofs                      |   X    |     X        |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Switch/Case                 |   X    |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Optional<T>                 |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Match expressions           |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- GADTs                       |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Modules*                     |        |              |          |Name lookup  |
+------------------------------+--------+--------------+----------+-------------+
|- Namespaces/packages         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Serialization/              |        |              |          |             |
|  deserialization ASTs        |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Inline functions            |        |              |          |SIL          |
+------------------------------+--------+--------------+----------+-------------+
|- Import Clang modules        |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Multiple-TU modules         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Error Handling /             |        |              |          |SIL          |
|Exceptions*                   |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- NSError interop             |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- POSIX interop               |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- C++ exceptions interop      |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Memory Management*           |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Weak pointers               |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Closures and cycles         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- nocapture                   |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Fragility Model*             |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Availability info           |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- API evolution               |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Conditional Compilation*     |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Objective-C                  |        |              |          |             |
|Interoperability*             |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Extensions as categories    |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Protocols                   |   X    |     X        |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Properties                  |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- CoreData                    |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- id and Class types          |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Constructors vs init        |   X    |     X        |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- API portability diags       |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Submodules support          |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Inline C functions          |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- CF interoperability         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Objective-C Extensions*      |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Typed collections           |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- API portability diags       |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Loading Swift modules       |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Extending Swift types       |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*C Interoperability*          |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Primitive type              |        |              |          |             |
|  conversions                 |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Preprocessor macros         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- "Thin" function pointers    |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Resource management         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- C strings                   |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Non-modularized             |        |              |          |             |
|  libraries                   |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Opaque types                |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*C++ Interoperability*        |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Value vs ref types          |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Value semantics             |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Exceptions                  |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Template instantiation      |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Inline functions            |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Constexpr                   |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Generics*                    |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Default implementations     |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Extending protocols         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Tuples                      |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Non-type parameters         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- <> in expressions           |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Strong Typedefs*             |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Generators*                  |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Actors*                      |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Key-Value Observing*         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Objective-C interop         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Type State*                  |        |              |          |SIL          |
+------------------------------+--------+--------------+----------+-------------+
|- pre & post conditions       |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Non-Null Pointers*           |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Reflection/Introspection*    |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Regular Expressions*         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Constant Expressions*        |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Immutability Model*          |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Container Literals*          |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Array literals              |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|- Dictionary literals         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Fixed-size Arrays*           |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+
|*Overload Resolution*         |        |              |          |             |
+------------------------------+--------+--------------+----------+-------------+

Compiler Engineering Tasks
--------------------------

* SIL

  * IRgen via SIL
  * Data-flow diagnostics (guaranteed initialization, guaranteed
    return, etc.)

* SIL Optimizations

  * Constant folding (integer overflow warnings, dead code
    elimination, etc.)
  * Inlining
  * Generics specialization
  * ARC optimization
  * LTO
  * NRVO
    
* Source Fidelity

  * "Perfect" AST pretty-printing
  * Source location information for every token written
  * "Implicit" annotations for AST nodes not written
  * Source locations for types

* Generics

  * Reimplement “conforms to protocol” to eliminate extraneous typing
  * Replace all informal protocols (e.g., for literals) with formal protocols
  * Parsing <> in an expression context
  * Constrained extensions, e.g.: extension <T:Printable, U:Printable> (T, U) : Printable
  * Variadic type variables, e.g.: extension <T...:Printable> (T...) : Printable
  * Kind polymorphism for protocols ::

        struct Foo : Fooable { func foo(x:Int) }
        struct Bar : Fooable { func foo<T>(x:T) }
        struct Bas : Fooable { func foo<T>(x:Zim<T>) }
        struct Blerg : Fooable { func foo<T, U, V>(x:Zang<T, U, V>) }
        protocol Fooable { func foo /* ??? */ }

* Debug Info

  * Basic line info
  * Basic variable/type information (until debugger work proceeds in earnest)

* Name Lookup

  * Rewrite type binding to cope with nesting, recursion, etc.
  * Implement name-hiding rules for modules, extensions
  * Rewrite with actual hash tables

* Type Checker

  * Diagnostics
  * Switch to new type checker, remove old type checking code
  * Performance
  * Overload resolution

* Driver

  * Build executables
  * Build shared libraries
  * "Module specification" that describes what to build

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
  * Generic values in structs/tuples
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
  * GUI REPL
  * Incremental code reloading

* Debugger

  * Integrate with REPL
  * Cross-language debugging

* API Evolution Checking Tool

* Static Analyzer

* Instruments

* Packaging and distribution tool

  * Command line
  * Xcode integration
  * SCM integration (e.g. "import" modules from github, ala http://blog.natefinch.com/2013/01/go-is-for-open-source.html)

* SDK Issues

  * Explicit properties
  * Explicit conformance to protocol
  * Consistent NS_ENUM/NS_OPTION use
  * Consistent ARC annotations  
  * Mark designated initializers
  * Mark APIs that take references to objects but don't retain them <rdar://problem/13275351>
  * Blocks

* Unit Testing

* Infrastructure

  * Demangler

* Other subsystems that need to be taught about Swift

  * Objective-C runtime
  * KVC subsystem

