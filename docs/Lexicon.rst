:orphan:

.. title:: Lexicon
.. default-role:: term

.. @raise litre.TestsAreMissing

This file defines several terms used by the Swift compiler and standard library
source code, tests, and commit messages. See also the `LLVM lexicon`_.

.. _LLVM lexicon: http://llvm.org/docs/Lexicon.html

.. note::

    This document uses Sphinx-specific features. If you are viewing this on
    GitHub, you'll have to use raw mode, or download and build the docs 
    yourself.

.. glossary::

  archetype
    A placeholder for a generic parameter or an associated type within a
    generic context. Sometimes known as a "rigid type variable" in formal
    CS literature. Directly stores its conforming protocols and nested 
    archetypes, if any.

  canonical SIL
    SIL after the
    `mandatory passes <mandatory passes / mandatory optimizations>` have run.
    This can be used as input to IRGen to generate LLVM IR or object files.

  Clang importer
    The part of the compiler that reads C and Objective-C declarations and
    exposes them as Swift. Essentially contains a small instance of Clang
    running inside the Swift compiler, which is also used during IRGen.

  conformance
    A construct detailing how a particular type conforms to a particular
    protocol. Represented in the compiler by the ProtocolConformance type at
    the AST level. See also `witness table`.

  contextual type
    1. The expected type for a Swift sub-expression based on the rest of the 
       statement. For example, in the statement ``print(6 * 9)``, the contextual
       type of the expression ``6 * 9`` is ``Any``.
    2. The type of a value or declaration from inside a potentially generic
       context. This type may contain `archetypes <archetype>` and cannot be 
       used directly from outside the context. Compare with `interface type`.

  DI (definite initialization / definitive initialization)
    The feature that no uninitialized variables, constants, or properties will
    be read by a program, or the analysis pass that operates on SIL to
    guarantee this. This was `discussed on Apple's Swift blog`__.
    
    __ https://developer.apple.com/swift/blog/?id=28

  existential
    A value whose type is a protocol composition (including a single protocol
    and *zero* protocols; the latter is the ``Any`` type).

  fragile
    Describes a type or function where making changes will break binary
    compatibility. See :doc:`LibraryEvolution.rst <LibraryEvolution>`.

  iff
    "`if and only if`__". This term comes from mathematics.
    
    __ https://en.wikipedia.org/wiki/If_and_only_if

  interface type
    The type of a value or declaration outside its generic context. These types
    are written using "formal" generic types, which only have meaning when
    combined with a particular generic declaration's "generic signature".
    Unlike `contextual types <contextual type>`, interface types store
    conformances and requirements in the generic signature and not in the types
    themselves. They can be compared across declarations but cannot be used
    directly from within the context.

  IUO (implicitly unwrapped optional)
    A type like Optional, but it implicitly converts to its wrapped type. If
    the value is ``nil`` during such a conversion, the program traps just as
    it would when a normal Optional is force-unwrapped. IUOs implicitly
    convert to and from normal Optionals with the same wrapped type.

  IWYU (include what you use)
    The accepted wisdom that implementation files (``.cpp``, ``.c``, ``.m``,
    ``.mm``) should explicitly ``#include`` or ``#import`` the headers they use.
    Doing so prevents compilation errors when header files are included in a
    different order, or when header files are modified to use forward
    declarations instead of direct includes.

  main module
    The module for the file or files currently being compiled.

  mandatory passes / mandatory optimizations
    Transformations over SIL that run immediately after SIL generation. Once
    all mandatory passes have run (and if no errors are found), the SIL is
    considered `canonical <canonical SIL>`.

  metatype
    The type of a value representing a type. Greg Parker has a good
    explanation of `Objective-C's "metaclasses"`__; because Swift has types
    that are *not* classes, a more general term is used.
    
    We also sometimes refer to a value representing a type as a "metatype
    object" or just "metatype", usually within low-level contexts like IRGen
    and LLDB. This is technically incorrect (it's just a "type object"), but
    the malapropism happened early in the project and has stuck around.
  
    __ http://sealiesoftware.com/blog/archive/2009/04/14/objc_explain_Classes_and_metaclasses.html
    
  model
    A type that conforms to a particular protocol. Sometimes "concrete
    model". Example: "Array and Set are both models of CollectionType".

  module
    Has *many* uses in the Swift world. We may want to rename some of them.
    #1 and #2 are the most common.
    
    1. A unit of API distribution and grouping. The ``import`` declaration 
       brings modules into scope. Represented as ModuleDecl in the compiler.
    2. A compilation unit; that is, source files that are compiled together.
       These files may contain cross-references. Represented as "the main
       module" (a specific ModuleDecl).
    3. (as "SIL module") A container for SIL to be compiled together, along
       with various context for the compilation.
    4. (as "LLVM module") A collection of LLVM IR to be compiled together.
       Always created in an LLVMContext.
    5. A file containing serialized AST and SIL information for a source file
       or entire compilation unit. Often "swiftmodule file", with "swiftmodule"
       pronounced as a single word.
    6. (as "Clang module") A set of self-contained C-family header files.
       Represented by a ClangModuleUnit in the Swift compiler, each of which is
       contained in its own ModuleDecl. For more information, see
       `Clang's documentation for Modules`__.
    7. Shorthand for a "precompiled module file"; effectively "precompiled
       headers" for an entire Clang module. Never used directly by Swift.
       See also `module cache`.
    
    __ http://clang.llvm.org/docs/Modules.html

  module cache
    Clang's cache directory for precompiled module files. As cache files, these
    are not forward-compatible, and so cannot be loaded by different versions
    of Clang (or programs using Clang, like the Swift compiler). Normally this
    is fine, but occasionally a development compiler will not have proper
    version information and may try to load older module files, resulting in
    crashes in ``clang::ASTReader``.

  NFC
    "No functionality change." Written in commit messages that are intended to
    have no change on the compiler or library's behavior, though for some this
    refers to having the *same* implementation and for others merely an
    *equivalent* one.  "NFC" is typically used to explain why a patch has no
    included testcase, since the Swift project requires testcases for all
    patches that change functionality.

  open existential
    An `existential` value with its dynamic type pulled out, so that the 
    compiler can do something with it.

  overlay
    A library that is imported whenever a C library or framework by the same
    name is imported. The purpose of an overlay is to augment and extend a
    library on the system when the library on the system cannot be modified.
    Apple has a number of overlays for its own SDKs in stdlib/public/SDK/.

  PR
    1. "Problem Report": An issue reported in `LLVM's bug tracker`__. 
       See also `SR`.
    2. "pull request"
    
    __ https://llvm.org/bugs/

  primary file
    The file currently being compiled, as opposed to the other files that are
    only needed for context. See also
    `Whole-Module Optimization <WMO (whole-module optimization)>`.

  QoI
    "Quality of implementation." The term is meant to describe not how
    well-engineered a particular implementation is, but how much value it
    provides to users beyond a sort of minimum expectation. Good diagnostics
    are a matter of QoI, as is good unoptimized performance. For example, a
    comment like "FIXME: QoI could be improved here" is suggesting that there's
    some sort of non-mandatory work that could be done that would improve the
    behavior of the compiler--it is not just a general statement that the code
    needs to be improved.

  Radar
    `Apple's bug-tracking system`__, or an issue reported on that system.
    
    __ https://bugreport.apple.com

  raw SIL
    SIL just after being generated, not yet in a form that can be used for
    IR generation.
    See `mandatory passes <mandatory passes / mandatory optimizations>`.

  resilient
    Describes a type or function where making certain changes will not break
    binary compatibility. See :doc:`LibraryEvolution.rst <LibraryEvolution>`.

  runtime
    Code that implements a language's dynamic features that aren't just
    compiled down to plain instructions. For example, Swift's runtime library
    includes support for dynamic casting and for the Mirror-based reflection.

  script mode
    The parsing mode that allows top-level imperative code in a source file.

  SIL
    "Swift Intermediate Language". A high-level IR used by the Swift compiler
    for flow-sensitive diagnostics, optimization, and LLVM IR generation.

  -sil-serialize-all
    A mode where all functions in a library are made available for inlining by
    any client, regardless of access control. Also called "magic performance
    mode" as a reminder of how this drastically changes compilation. Not
    guaranteed to work on arbitrary code.

  SR
    An issue reported on `bugs.swift.org <https://bugs.swift.org>`_. A
    backronym for "Swift Report"; really the name is derived from LLVM's
    idiomatic use of "PR" ("Problem Report") for its bugs. We didn't go with
    "PR" for Swift because we wanted to be able to unambiguously reference
    LLVM bugs.

  stdlib
    "Standard library". Sometimes this just means the "Swift" module (also
    known as "swiftCore"); sometimes it means everything in the stdlib/
    directory. Pronounced "stid-lib" or "ess-tee-dee-lib".

  trap
    A deterministic runtime failure. Can be used as both as a noun ("Using an
    out-of-bounds index on an Array results in a trap") and a verb
    ("Force-unwrapping a nil Optional will trap").

  type metadata
    The runtime representation of a type, and everything you can do with it.
    Like a ``Class`` in Objective-C, but for any type.

  USR
    A Unified Symbol Resolution (USR) is a string that identifies a particular
    entity (function, class, variable, etc.) within a program. USRs can be
    compared across translation units to determine, e.g., when references in
    one translation refer to an entity defined in another translation unit.

  value witness table
    A runtime structure that describes how to do basic operations on an unknown
    value, like "assign", "copy", and "destroy". (For example, does copying
    this value require any retains?)

    Only conceptually related to a `witness table`.

  vtable (virtual dispatch table)
    A map attached to a class of which implementation to use for each
    overridable method in the class. Unlike an Objective-C method table,
    vtable keys are just offsets, making lookup much simpler at the cost of
    dynamism and duplicated information about *non*-overridden methods.

  witness
    The value or type that satisfies a protocol requirement.

  witness table
    The SIL (and runtime) representation of a `conformance`; essentially a
    `vtable <vtable (virtual dispatch table)>` but for a protocol instead of
    a class.

    Only conceptually related to a `value witness table`.

  WMO (whole-module optimization)
    A compilation mode where all files in a module are compiled in a single
    process. In this mode there is no `primary file`; all files are parsed,
    type-checked, and optimized together at the SIL level. LLVM optimization
    and object file generation may happen all together or in separate threads.
