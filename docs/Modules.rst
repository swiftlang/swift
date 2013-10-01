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


Modules can "re-export" other modules
-------------------------------------

Like any other body of code, a module may depend on other modules in its
implementation. The module implementer may also choose to `re-export` these
modules, meaning that anyone who imports the first module will also have access
to the declarations in the re-exported modules. ::

  import [exported] ChessAIs

As an example, the "Cocoa" `framework` on OS X exists only to re-export three
other frameworks: AppKit, Foundation, and CoreData.

Just as certain declarations can be selectively imported from a module, so too
can they be selectively re-exported, using the same syntax::

  import [exported] func ChessAIs.createGreedyPlayer

.. admonition:: TODO

  This is currently implemented using a dedicated ``[exported]`` keyword, but is
  likely to end up using the access control syntax we eventually design for
  regular declarations.


Modules are uniquely identified by their full name
--------------------------------------------------

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


``import``
==========

As shown above, a module is imported using the ``import`` keyword, followed by
the name of the module::

  import AppKit

To import only a certain declaration from the module, you use the appropriate
declaration keyword::

  import class AppKit.NSWindow
  import func AppKit.NSApplicationMain
  import var AppKit.NSAppKitVersionNumber
  import typealias AppKit.NSApplicationPresentationOptions

- ``import typealias`` has slightly special behavior: it will match any type
  other than a protocol, regardless of how the type is declared in the imported
  module.
- ``import class``, ``struct``, and ``enum`` will succeed even if the
  name given is a typealias for a type of the appropriate kind.
- ``import func`` will bring in all overloads of the named function.

.. note::

  The best way to think about the declaration keyword used with ``import`` is
  that it specifies how *you* want to use the declaration you are importing,
  rather than how the module writer declared it. This also provides some amount
  of API stability, though at the ABI level changing the canonical name or kind
  for a type is still a binary-incompatible change.

.. admonition:: TODO

  There is currently no way to selectively import extensions or operators.


Multiple source files
---------------------

Most programs are broken up into multiple source files, and these files may
depend on each other. To facilitate this design, declarations in *all* source
files in a module (including the "main module" for an executable) are implicitly
visible in each file's context. It is almost as if all these files had been
loaded with ``import``, but with a few important differences:

- The declarations in other files belong to the module being built, just like
  those in the current file. Therefore, if you need to refer to them by
  qualified name, you need to use the name of the module being built.
- A module is a fully-contained entity: it may depend on other modules, but
  those other modules can't depend on it. Source files within a module may
  have mutual dependencies.

.. note::

  The current plan is to have two possible implementations for this. A serial
  compilation process would pass all source files to the compiler in a single
  invocation; parallelism nice-to-have.
  
  The more complicated process feeds a list of all files in a target to the
  compiler. These files are parsed but not type-checked; "lazy" type-checking
  will be used when the compiler needs to refer to declarations in these files.
  Once compiled, a serialized form of the source file could be used to avoid
  having to reparse that particular file, but this is just an optimization.
  
  A more detailed description of the build system plan will be available in a
  separate doc.

.. admonition:: TODO

  None of this works yet, and indeed has not yet been agreed upon.

.. admonition:: FIXME

  This wouldn't belong in the user model at all except for the implicit 
  visibility thing. Is there a better way to talk about this?


Ambiguity
---------

Because two different modules can declare the same name, it is sometimes
necessary to use a `qualified name` to refer to a particular declaration::

  import Chess
  import Xiangqi

  if userGame == "chess" {
    Chess.playGame()
  } else if userGame == "xiangqi" {
    Xiangqi.playGame()
  }

Here, both modules declare a function named ``playGame`` that takes no
arguments, so we have to disambiguate by "qualifying" the function name with
the appropriate module.

These are the rules for resolving name lookup ambiguities:

1. Declarations in the current source file are better than imported 
   declarations.
2. Declarations from selective imports are better than declarations from
   non-selective imports. (This may be used to give priority to a particular
   module for a given name.)
3. Declarations from other files in the same module [#]_ are better than
   declarations from non-selective imports, but worse than declarations from
   selective imports.
4. Every source file implicitly imports the core standard library as a
   non-selective import.
5. If the name refers to a function, normal overload resolution may resolve
   ambiguities.

.. [#] FIXME: not implemented yet, since the main feature hasn't been
       implemented either.


Hierarchical Module Names
-------------------------

The purpose of modules is to provide declarations for source code to use; it
does so by introducing names into the source file's context. In addition to
the contents of a module or a particular selectively-imported decl, the name
of the module itself is also introduced into the translation unit. This is
how qualified names are resolved by the compiler.

In the case of a hierarchical module name (like "Com.Frantic.Epilogue"),
the compiler will introduce *two* names into the current scope: the full 
three-part name, and the last component of the module. Therefore, a class
Com.Frantic.Epilogue.EditController can be referred to as "EditController",
"Epilogue.EditController", or "Com.Frantic.Epilogue.EditController".

Note that an import must always use the fully qualified name; that is,
this is not allowed::

  import Com.Frantic.Epilogue
  import class Epilogue.EditController // error: "'Epilogue' module not found"

Because access into a module and access into a type look the same, it is bad
style to declare a type with the same name as a top-level module used in your
program, or with the same fully-qualified name as a separate module::

  // Example 1:
  import Foundation
  import struct BuildingConstruction.Foundation
  
  var firstSupport = Foundation.SupportType() // from the struct or from the module?


  // Example 2:
  import /*module*/ Com.Frantic.Epilogue.Console
  import class Com.Frantic.Epilogue.Console
  
  Com.Frantic.Epilogue.Console.requireXTerm() // from the class or from the module?

In both cases, the type takes priority over the module, but this should still 
be avoided.


Submodules
----------

.. admonition:: FIXME

  Write this section. Submodules are basically like hierarchical modules except
  that they live in the top-level module's file.


Import Search Paths
-------------------

.. admonition:: FIXME

  Write this section. Can source files be self-contained modules? How does -i
  mode work? Can the "wrong" module be found when looking for a dependency
  (i.e. can I substitute my own Foundation and expect AppKit to work)?
  How are modules stored on disk? How do hierarchical module names work?


Interoperability with Objective-C via Clang
===========================================

.. admonition:: FIXME

  Write this section. Objective-C modules can already be imported into Swift.
  We definitely want to be able to import individual Objective-C headers into
  Swift. We probably need to be able to import Swift sources into Objective-C,
  and we probably need that to work even within a single target (for
  migration/coexistence purposes). We could do the latter with header files,
  but that would be pretty kludgy. We potentially have a real problem with
  mutual dependencies that cross language boundaries.


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

  framework
    A mechanism for library distribution on OS X. Traditionally contains header
    files describing the library's API, a binary file containing the
    implementation, and a directory containing any resources the library may
    need.

    Frameworks are also used on iOS, but as of iOS 7 custom frameworks cannot
    be created by users.

  import
    To locate and read a module, then make its declarations available in the 
    current context.

  library
    Abstractly, a collection of APIs for a programmer to use, usually with a
    common theme. Concretely, the file containing the implementation of these
    APIs.

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

  re-export
    To directly expose the API of one module through another module. Including
    the latter module in a source file will behave as if the user had also
    included the former module.

  serialized module
    A particular encoding of a module that contains declarations that have 
    already been processed by the compiler. It may also contain implementations 
    of some function declarations in `SIL` form.
  
  SIL
    "Swift Intermediate Language", a stable IR for the distribution of
    inlineable code.
  
