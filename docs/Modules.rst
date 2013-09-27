.. @raise litre.TestsAreMissing
.. default-role:: term

===================
Modules User Model
===================

A `module` is the primary unit of code sharing in Swift. This document
describes the experience of using modules in Swift: what they are and what they
provide for the user.

.. contents:: :local:


High-Level Overview
===================

A module contains declarations
------------------------------

The primary purpose of a module is to provide declarations of types, functions,
and global variables that are present in a library. `Importing <import>` the
module gives access to these declarations and allows them to be used in your
code.

::

  import Chess
  import Foundation

You can also selectively import certain declarations from a module::

  import func ChessAIs.createGreedyPlayer
  import class AVFoundation.AVPlayer

.. admonition:: Comparison with Other Languages

  Importing a module is much like importing a library in Ruby, Python, or Perl,
  importing a class in Java, or including a header file in a C-family language. 
  However, unlike C, module files are not textually included and must be valid
  programs on their own, and may not be in a textual format at all. Unlike Java,
  declarations in a module are not visible at all until imported. And unlike the
  dynamic languages mentioned, importing a module cannot automatically cause
  cause any code to be run.


Imported declarations can be accessed with qualified or unqualified lookup
--------------------------------------------------------------------------


Once a module has been imported, its declarations are available for use within
the current source file. These declarations can be referred to by name, or
by `qualifying <qualified name>` them with the name of the module::

  func playChess(blackPlayer : Chess.Player, whitePlayer : Chess.Player) {
    var board = Board() // refers to Chess.Board
  }


Modules provide a unique context for declarations
-------------------------------------------------

A declaration in a module is unique; it is never the same as a declaration with
the same name in another module (with one caveat described below). This means
that two types ``Chess.Board`` and ``Xiangqi.Board`` can exist in the same
program, and each can be referred to as ``Board`` as long as the other is not
visible. If more than one imported module declares the same name, the full
`qualified name` can be used for disambiguation.

.. note::

  This is accomplished by including the module name in the `mangled name` of a
  declaration. Therefore, it is an ABI-breaking change to change the name of a
  module containing a public declaration.

.. warning::

  The one exception to this rule is declarations that must be compatible with
  Objective-C. Such declarations follow the usual Objective-C rules for name
  conflicts: all classes must have unique names, all protocols must have unique
  names, and all constructors, methods, and properties must have unique names 
  within their class (including inherited methods and properties).


Modules are uniquely identified by their name
---------------------------------------------

Module names exist in a global namespace and must be unique. To this end,
third-party library vendors are encouraged to name modules using a 
`reverse-DNS`__ scheme. For example, if you work for a company named "Frantic"
with the website "frantic.com" and you are releasing a library called 
"Epilogue", your module name should be "Com.Frantic.Epilogue". This helps
ensure that your module name will not conflict with any others on the system.

__ http://goto.apple.com/?http://en.wikipedia.org/wiki/Reverse_domain_name_notation

Like type names, module names are conventionally capitalized.

.. admonition:: TODO

  Hierarchical module names don't actually work yet, and may not work at all 
  for Swift 1.0, since building and shipping frameworks will not be supported.


Modules may contain code
------------------------

In addition to declarations, modules may contain implementations of the
functions they define. The compiler may choose to use this information when
optimizing a user's program, usually by inlining the module code into a caller.
In some cases [#]_, the compiler may even use a module's function 
implementations to produce more effective diagnostics.

Modules can also contain `autolinking` information, which the compiler passes
on to the linker. This can be used to specify which library implements the
declarations in the module.

.. [#] Specifically, code marked with the ``[transparent]`` attribute is
   required to be "transparent" to the compiler: it *must* be inlined and
   will affect diagnostics.


Glossary
========

.. glossary::

  autolinking
    A technique where linking information is included in compiled object files,
    so that external dependencies can be recorded without having to explicitly
    specify them at link time.

  Clang module
    A module whose contents are generated from a C-family header or set of 
    headers. See Clang's Modules__ documentation for more information.
  
    __ http://goto.apple.com/?http://clang.llvm.org/docs/Modules.html

  import
    To locate and read a module, then make its declarations available in the 
    current context.

  mangled name
    A unique, internal name for a type or value. The term is most commonly used
    in C++; see Wikipedia__ for some examples. Swift's name mangling scheme is
    not the same as C++'s but serves a similar purpose.

    __ http://goto.apple.com/?http://en.wikipedia.org/wiki/Name_mangling#Name_mangling_in_C.2B.2B

  module
    An entity containing the API for a library, to be `imported <import>` into
    a source file.

  qualified name
    A multi-piece name like ``Foundation.NSWindow``, which names an entity
    within a particular context. This document is concerned with the case where
    the context is the name of an imported module.

  serialized module
    A particular encoding of a module that contains declarations that have 
    already been processed by the compiler. It may also contain implementations 
    of some function declarations in `SIL` form.
  
  SIL
    "Swift Intermediate Language", a stable IR for the distribution of
    inlineable code.
  
