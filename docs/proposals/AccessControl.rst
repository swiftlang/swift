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

The types of all parameters and the return type of a func marked
``@public`` (including the implicit ``self`` of methods) must also be
``@public``.

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
``@public``\ -ness of any compound type, including function types,
tuple types and instances of generic types: The compound type is
public if and only if all of the component types, are ``@public`` and
either defined in this module or re-exported from this module.

Enums
=====

The cases of an ``enum`` are ``@public`` if and only if the ``enum``
is declared ``@public``.

Derived Classes
===============

A method that overrides an ``@public`` method must be declared
``@public``, even if the enclosing class is non-``@public``.

Protocols
=========

A ``@public`` protocol can have ``@public`` and non-``@public``
requirements.  ``@public`` requirements can only be satisfied by
``@public`` declarations. Non-``@public`` requirements can be
satisfied by ``@public`` or non-``@public`` declarations.

Conformances
============

The conformance of a type to a protocol is ``@public`` if that
conformance is part of an ``@public`` declaration.  The program is
ill-formed if any declaration required to satisfy a ``@public``
conformance is not also declared ``@public``.::

  @public protocol P {
    @public func f() { g() }
    func g()
  }

  struct X : P { // OK, X is not @public, so neither is its 
    func f() {}  // conformance to P, and therefore f
    func g() {}  // can be non-@public
  }

  protocol P1 {}

  @public struct Y : P1 {} // Y is @public so its 
                           // conformance to P1 is, too.

  @public
  extension Y : P {     // This extension is @public, so
    @public func f() {} // Y's conformance to P is also, and
    func g() {}         // thus f must be @public too
  }

  protocol P2 {}

  extension Y : P2 {}   // Y's conformance to P2 is non-@public

.. Note:: It's our expectation that in the near term, and probably for
  v1.0, non-``@public`` conformances on ``@public`` types will be
  diagnosed as ill-formed/unsupported.

A Related Naming Change
=======================

The existing ``@exported`` attribute for imports should be renamed
``@public`` with no change in functionality.
          
Future Directions
=================

Some obvious directions to go in this feature space, which we are not
proposing today, but with which we tried to make this proposal
compatible:

* non-``@public`` conformances
* file-private accessibility
* explicit non-``@public`` overrides, e.g. ``@!public``

