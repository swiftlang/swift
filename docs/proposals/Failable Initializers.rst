:orphan:

This is an intentionally simple proposal for representing failable
initializers and construction failure. This only addresses the very
simple case of an initializer returning null; it doesn't try to be
a complete error handling solution.

The ``fail`` statement
=======================

If a function returns a ``T?`` type, it may use the statement
``fail``, which in a normal function is sugar for ``return .None``.
``fail`` has special behavior in failable initializer definitions,
described below.

Failable initializers
=====================

An initializer that may fail can declared with the syntax
``init?(<args>)``. A normal ``return`` or fall-off-the-end of
a failable initializer produces the constructed ``self`` value
wrapped in a ``T?``. A ``fail`` statement in a failable
initializer causes the ``self`` value being constructed to be
discarded, and the initializer produces ``.None``::

  class Foo {
    var resource : LimitedResource
    
    init?() {
      switch acquireLimitedResource() {
      case .Some(let resource):
        self.resource = resource
      case .None:
        fail
      }
    }
  }

The semantics of discarding ``self`` in an initializer are described in the
`Discarding self`_ section below.
  
A construction expression that references a failable initializer
evaluates to an Optional of that type::

  switch Foo() {
  case .Some(var foo):
    // Use foo
  case .None:
    println("Failed to acquire resource")
    exit(1)
  }

Delegating between failable initializers
========================================

If an initializer delegates to a failable initializer by invoking
``self.init`` or ``super.init``, that initializer must also be
failable. If the referenced initializer fails, that failure is
implicitly propagated through the delegator, and the self value
is destroyed. It is an error if a non-failable initializer
invokes ``self.init`` or ``super.init`` on a non-failable initializer.

Discarding self
===============

When an initializer ``fail``\-s, the object being constructed must be cleaned
up. How this happens depends on which stage of initialization the failure
occurs in:

- If ``fail`` is executed before ``self`` is definitely initialized, the stored
  properties of ``self`` that have been initialized are destroyed, and any
  memory allocated for ``self`` is deallocated. ``self``'s own destructor,
  if any, is not invoked.
- If ``fail`` is executed after a class instance is definitely initialized,
  then ``self`` is **released**. Normally, this will invoke the destructor
  of the fully-derived class, unless initialization caused the ``self``
  reference to escape, in which case the object remains alive, although it
  is not returned from the initializer. For value types, ``self`` is destroyed
  normally.

Importing Objective-C initializers
==================================

Objective-C init methods may potentially fail and return ``nil`` as
a matter of course. An ``init`` method imported from Objective-C
should be imported as an ``@unchecked init?(...)`` method, and
referencing such an initializer produces an ``@unchecked T?`` value.
``@unchecked`` failable initializers may be delegated to by non-
failable initializers in Swift, in which case the delegated-to
initializer is dynamically asserted not to fail, or by failable
initializers, in which case failure is checked for and propagated
as for normal checked failable initializers.


-Joe
