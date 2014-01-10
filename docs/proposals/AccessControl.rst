:orphan:

================================
 Basic Access Control for Swift
================================

:Authors: Dave Abrahams, Jordan Rose, Doug Gregor

We propose the minimum level of access control sufficient to allow the
standard library to distinguish public API from implementation detail
without uglifying code with underscore prefixes.  These distinctions
are necessary during standard library development for the same reasons
they're needed by users: without a minimum level of modularity, any
codebase quickly becomes brittle and difficult to evolve.  The ability
to rapidly evolve the standard library is especially important at this
time.  The proposal is designed to be compatible with desired future
directions, which are discussed at the end.

The ``@public`` Attribute
=========================

We propose a new attribute, ``@public``, that can adorn any
declaration not local to a function.  For the purpose of standard
library development, even just parsing this attribute without
implementing semantics would be extremely useful in the near term.

Basic Semantics
===============

``@public`` makes a declaration visible in code where the enclosing
module is imported.  So, given this declaration in the ``Satchel``
module::

  @public struct Bag<T> : ... {
   ...
  }

We could write, in any other module, ::

  import Satchel
  typealias SwingMe = Bag<Cat>

The difference from the status quo being that without ``@public`` on
the declaration of ``Bag``, the use of ``Bag`` above would be
ill-formed.

Type-Checking
=============

All parameters to a ``func`` marked ``@public`` (including the
implicit ``self`` of methods) must also be ``@public``::

  struct X {}                   // not @public
  @public struct Y {}
  func f(_: X) {}               // OK; also not @public
  @public func g(_: Y) {}       // OK; uses only @public types
  @public func h(_: X, _: Y) {} // Ill-formed; non-public X in public signature

A ``typealias`` marked ``@public`` must refer to a type marked
``@public``::

  typealias XX = X              // OK; not @public
  @public typealias YY = Y      // OK; Y is @public
  @public typealias XXX = X     // Ill-formed; public typealias refers to non-public type

There is a straightforward and obvious rule for composing the
``@public``\ -ness of a tuple type, which extends naturally to
function types: a tuple type is ``@public`` if all of its components
are ``@public``.  An instance of a generic type is ``@public`` if the
generic type itself, and all of its arguments, are declared
``@public``.  If the types involved come from different modules, all
of the modules concerned must be imported (directly or via re-export)
in order to make the result accessible.

Public Conformances
===================

[It's our expectation that in the near term, and probably for v1.0,
the language will *only* support ``@public`` conformances, and
conformances that would otherwise be non-public will be diagnosed as
ill-formed/unsupported.]

We don't think it's necessary to allow conformances to be explicitly
declared ``@public`` other than via the public extension mechanism
described in the next section.  However, there is a language rule
associated with ``@public`` conformances: all of the API required to
satisfy a ``@public`` conformance must also be declared ``@public``.


Public Extensions
=================

When an extension is declared ``@public``, all conformances and
functions declared therein are implicitly ``@public``::

  @public struct X {
    func helper()  {...}         // Only accessible within this module
  }

  @public extension X : Stream { // X's conformance to Stream
    func next() -> T? { ... }    // and its next() method are visible
  }                              // wherever this module is imported

Yes, that makes extensions different from ``class``, ``struct``, and
``enum``, but we think it's the right tradeoff.

Future Extensions
=================

Some obvious directions to go in this feature space, which we are not
proposing today, but with which we tried to make this proposal
compatible:

* non-``@public`` conformances
* file-private accessibility
* explicit non-``@public`` overrides, e.g. ``@!public``

