Scope and introduction
======================

This documents defines the policy for applying access control modifiers and
certain naming conventions for the Swift standard library and overlays.

In this document, 'stdlib' refers to core standard library and overlays for
system framework written in Swift.

Currently Swift compiler has a notion of access control --- private, internal
and public.  Access control in Swift, as it is implemented now, is only
concerned with API-level issues, not ABI.  Stdlib does not have a stable ABI,
and is compiled in "non-resilient" mode with inlining into user code, thus, all
stdlib symbols are considered ABI and stdlib users should be recompiled after
*any* change to the stdlib.

`public` modifier
=================

User-visible APIs should be marked public.

Unfortunately, currently the compiler has bugs that force stdlib implementation
to work around them, defining additional symbols.  Sometimes these symbols have
to be public because of access control rules, for example::

  // Workaround.
  public protocol _Collection { ... }

  // Symbol intended for use outside stdlib.
  public protocol Collection : _Collection { ... }

In order to clearly mark such symbols, they should be named using the
underscore rule.

Access control in Swift does not define a notion of SPI.  When the stdlib needs
to define an SPI that is defined and used in different modules, this
stdlib-internal SPI should be marked public because of access control rules.
In order to distinguish SPIs from APIs, SPI symbol names should be named using
the underscore rule.

`internal` modifier
===================

`internal` modifier is used in stdlib with usual semantics.  `internal` is the
implied default, and should be omitted where possible.

For consistency with the use of underscore rule for `public` declarations,
`private` and `internal` symbols should be named using the underscore rule in
the following cases:

* module-scope `private` and `internal` symbols:

  var _internalStdlibConstant: Int { ... }

* `private` and `internal` symbols nested within `public` types::

  public struct Dictionary {
    var _representation: _DictionaryRepresentation
  }

`private` modifier
==================

`private` modifier can not be used in stdlib at least until rdar://17631278 is
fixed.

Symbol underscoring rule
========================

Variables, functions and typealiases should have names that start with an
underscore::

  var _value: Int
  func _bridgeSomethingToAnything(something: AnyObject) -> AnyObject
  typealias _InternalTypealias = HeapBuffer<Int, Int>

In order to apply the underscoring rule to an initializer, one of its label
arguments *or* internal parameter names should start with an underscore::

  public struct Foo {
    init(_count: Int) {}
    init(_ _otherInitializer: Int) {}
  }

Note: the identifier that consists of a single underscore ``_`` is not
considered to be a name that starts with an underscore.  For example, this
initializer is public::

  public struct Foo {
    init(_ count: Int) {}
  }

The compiler and IDE tools may use the underscore rule, combined with
additional heuristics, to hide stdlib symbols that users don't need to see.

Users are prohibited to use underscored symbols in their source code, even if
these symbols are visible through compiler diagnostics or IDE tools.

