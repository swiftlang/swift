.. @raise litre.TestsAreMissing
.. default-role:: term
.. title:: Modules Build Model


This document describes the process used to build a module from multiple Swift
source files.

.. contents:: :local:

See also the :doc:`Modules User Model <Modules>`.


Build Setups
============

Building a single source file
-----------------------------

A single Swift source file can be compiled to an object file using ``swift -c``.

.. sidebar:: Why is executable mode the default?

  By making executable mode the default, a single-file script can go from being
  interpreted to being compiled without any extra work. 

.. _executable-mode:
.. _library-mode:

By default, a file is compiled in "executable" mode, which

- permits top-level code, to be run via ``main`` when the program is launched,
  and
- does not generate global symbols for top-level variable declarations.

Files can also be compiled in "library" mode, which reverses these restrictions
(forbidding top-level code but allowing the declaration of module-scoped global
variables). This is accomplished by passing the ``-parse-as-library`` flag to
the compiler.

.. admonition:: FIXME

  Not allowing true globals in the main source file seems like an awkward 
  limitation. The reason for this is that top-level variables should follow
  local variable definite initialization rules, but it seems like you might
  want to define true globals as well.


Building an entire module at once
---------------------------------

Passing multiple source files to the Swift compiler will compile them all to a 
single object file. Each file will be individually parsed and contains its own
context, but non-private declarations in each file are :ref:`implicitly visible 
<implicit-visibility>` to every other file at the global scope.

Passing multiple files to the compiler implies :ref:`library mode
<library-mode>`.

.. admonition:: TODO

  The compiler ought to be smart enough to figure out if there is one main
  source file and several library files. This would help the command line
  experience going from an interpreted script, spread over multiple files, to
  a compiled binary.


.. _module-source-list:

Building a module in pieces
---------------------------

It's not usually desired to rebuild all source files all the time, so a library
can also be built by running the compiler separately for each source file. In
this case, the other files in the module are still parsed in every invocation
of the compiler (for implicit visibility purposes), but code is only
generated for the main source file.

The other source files in the module are specified using a `compilation 
database`__ passed to the compiler with the ``-module-source-list`` flag.
The ``command`` field of each entry may be omitted. Other files in the module
are parsed in :ref:`library mode <library-mode>`.

__ http://goto.apple.com/?http://clang.llvm.org/docs/JSONCompilationDatabase.html

.. admonition:: TODO

  In the long run we want a cleaner way to do this that's more compatible with,
  say, make. Ideally we'd also like a "simple" mode where module boundaries
  are derived from filesystem structure.


Interpreted Mode
----------------

The Swift compiler also supports being used as an interpreter through use of
the ``-i`` flag. In this mode, the single input file is parsed in
:ref:`executable mode <executable-mode>`. As with compilation, files can be
explicitly imported within the source using ``import``, or made implicitly
visible through the use of a :ref:`module source list <module-source-list>`.
However, in interpreter mode *all imported source files* are processed for code
generation, since there's no chance to link separate object files later.

Swift source files support the Unix convention of a `shebang`__ line at the
top of the file; this line will be recognized and skipped in the compiler's
interpreter mode.

__ http://goto.apple.com/?http://en.wikipedia.org/wiki/Shebang_(Unix)


Build Details
=============

.. admonition:: FIXME

  Write this section. There should be some kind of cache for the files in the
  build, so that they don't need to be reparsed every time. See if we can take
  advantage of the serialized ASTs in object files for this. We also need to
  merge the serialized ASTs---in the simplest case, we'll just put them all
  together in a swiftmodule or in the generated binary, but we should also
  consolidate the types and resolve any internal decl refs.
  
  Also, Xcode needs to expose other targets as modules.


Objective-C Interoperability
============================

.. admonition:: FIXME

  Write this section. Across module boundaries is the easy case: they just look
  like any other modules. (Though, note: how does this work for user 
  frameworks?) So, how to:
  
  - load Objective-C from Swift? (do we need an explicit module map? do we get
    implicit visibility? who knows?)
  - load Swift from Objective-C? (what do you @import? if that depends on Clang
    modules, how to get the same decls? if it depends on other sources in the
    same target, how to get the same decls?)
  - deal with mutual dependencies? (supposedly, everything imported from Clang
    has been fully type-checked. Clang won't even have a way to type-check
    things from Swift, but Swift doesn't have forward declarations.)
