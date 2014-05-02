:orphan:

This is an intentionally simple proposal for handling Objective-C initializers
that can fail and return nil. it doesn't try to support constructor failure
in Swift classes or be at all a complete error handling solution.

Failable initializers
=====================

A **failable initializer** can be declared by specifying its return type to be
an optional or implicitly unwrapped optional type. The base type of the optional must be
``Self``, to indicate a whole-object initializer, or the declared type, to
indicate a subobject initializer. Only classes that inherit from ``@objc`` base
classes may declare failable initializers. For example::

  class Foo: NSView {
    init() -> Foo? {
      super.init()
    }

    init(withCoder coder: NSCoder) -> Self? {
      super.init(withCoder: coder)
    }
  }

An initializer that returns Optional
is a **checked failable initializer**, and one that returns ``@unchecked``
Optional is an **unchecked failable initializer**. 
Failable whole-object or subobject initializers have all the usual
requirements and behavior of non-failable initializers but with additional
delegation behavior, described below. Expressions that construct
class instances using failable initializers have Optional or ``@unchecked``
Optional type of the constructed type, and result in ``nil`` if the
initialization fails.

Delegating to failable initializers
===================================

Initialization failure cannot be initiated in Swift but can be encountered by
delegating to an initializer written in Objective-C.  If a failable initializer
delegates to a failable initializer by invoking ``self.init`` or
``super.init``, and the referenced initializer fails by returning ``nil``, then
that failure is implicitly propagated through the delegator, the self value is
destroyed, and the delegator also returns ``nil``. It is an error if a
non-failable initializer delegates to a checked failable initializer.  If an
non-failable initializer delegates to an unchecked failable initializer, a
runtime check is performed in the delegator after the delegatee initializer is
completed. A runtime failure occurs if the delegatee returned nil.

Importing Objective-C initializers
==================================

Objective-C init methods may potentially fail and return ``nil`` as
a matter of course, so all ``init`` methods imported from Objective-C
will be imported as ``@unchecked init(...)`` methods.
Unchecked failable initializers may be delegated to by non-
failable initializers in Swift, in which case the delegated-to
initializer is dynamically asserted not to fail, or by failable
initializers, in which case failure is checked for and propagated
as for normal checked failable initializers.
