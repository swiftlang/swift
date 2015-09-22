:orphan:

Failable initializers
=====================

A **failable initializer** can return early with an error, without having
initialized a new object. Examples can include initializers which validate
input arguments, or attempt to acquire a limited resource.

There are two types of failable initializers:

- An initializer can be declared as having an optional return type, in
  which case it can signal failure by returning nil.

- An initializer can be declared as throwing, in which case it can signal
  failure by throwing an error.

Convenience initializers
------------------------

Failing convenience initializers are the easy case, and are fully supported
now. The failure can occur either before or after the self.init()
delegation, and is handled as follows:

  #. A failure prior to the self.init() delegation is handled by freeing the
     fully-uninitialized self value.

  #. A failure after the self.init() delegation is handled by freeing the
     fully-initialized self.value.

Designated initializers
-----------------------

Failing designated initializers are more difficult, and are the subject of this
proposal.

Similarly to convenience initializers, designated initializers can fail either
before or after the super.init() delegation (or, for a root class initializer,
the first location where all instance properties become initialized).

When failing after the super.init() delegation, we already have a
fully-initialized self value, so releasing the self value is sufficient. The
user-defined deinitializer, if any, is run in this case.

A failure prior to the super.init() delegation on the other hand will leave us
with a partially-initialized self value that must be deallocated. We have to
deinitialize any instance properties of self that we initialized, but we do
not invoke the user-defined deinitializer method.

Description of the problem
--------------------------

To illustrate, say we are constructing an instance of a class C, and let
superclasses(C) be the sequence of superclasses, starting from C and ending
at a C_n that is either a root class or an Objective-C class:

::

  superclasses(C) = {C, C_1, C_2, ..., C_n}

Suppose our failure occurs in the designated initializer for class C_k. At this
point, the self value looks like this:

  #. All instance properties in ``{C, ..., C_(k-1)}`` have been initialized.
  #. Zero or more instance properties in ``C_k`` have been initialized.
  #. The rest of the object ``{C_(k+1), ..., C_n}`` is completely uninitialized.

In order to fail out of the constructor without leaking memory, we have to
destroy the initialized instance properties only, then deallocate the value.

Furthermore, we have to deallocate the object by performing a peer delegation
to the -dealloc method, in case C_n overrides -alloc, implementing a memory
pool for example.

Possible solutions
------------------

One approach is to think of the super.init() delegation as having a tri-state
return value, instead of two-state:

  #. First failure case -- object is fully initialized
  #. Second failure case -- object is partially initialized
  #. Success

This is problematic because now the ownership conventions in the initializer
signature do not really describe the initializer's effect on reference counts;
we now that this special return value for the second failure case, where the
self value looks like it should have been consumed but it wasn't.

It is also difficult to encode this tri-state return for throwing initializers.
One can imagine changing the try_apply and throw SIL instructions to support
returning a pair (ErrorType, AnyObject) instead of a single ErrorType. But
this would ripple changes throughout various SIL analyses, and require IRGen
to encode the pair return value in an efficient way.

The proposed solution
---------------------

A simpler approach seems to be to introduce a new partialDeinit entry point,
referenced through a special kind of SILDeclRef. This entry point is dispatched
through the vtable and invoked using a standard class_method sequence in SIL.

This entry point's job is to conditionally deinitialize instance properties
of the self value, without invoking the user-defined deinitializer.

When a designated initializer for class C_k fails prior to performing the
super.init() delegation, we emit the following code sequence:

  #. First, de-initialize any instance variables this initializer may have
     initialized.
  #. Second, invoke ``partialDeinit(self, M)``, where M is the static
     metatype of ``C_k``.

The partialDeinit entry point is implemented as follows:

  #. If the static self type of the entry point is not equal to M, first
     delegate to the superclass's partialDeinit entry point, then
     deinitialize all instance properties in ``C_k``.

  #. If the static self type is equal to M, we have finished deinitializing
     the object. Make a call to a runtime function,
     ``swift_partialDeallocInstance()``, passing in both self and M. This function
     walks up the superclass chain until it finds an Objective-C class ``C_n``,
     and invokes ``C_n``'s implementation of ``-dealloc``.

We delegate to the superclass partialDeinit entry point before doing our own
deinitialization, to ensure that instance properties are deinitialized in the
reverse order in which they were initialized. This might not matter.

Note that if C_n has a failing initializer, then either ``C_{n+1}`` has a
failing initializer, or ``C_{n+1}`` invokes ``C_n``'s initializer with
``super.init!`` or ``try!``, trapping on failure. In the latter case, ``C_{n+1}``
still needs a partialDeinit entry point, even though it does not have any
failing initializers of its own.

The one case where a class does not need a partialDeinit entry point is if it
has no stored properties of its own.

Implementation concerns
-----------------------

The bulk of this feature would be driven from DI. Right now, DI only implements
failing designated initializers in their full generality for structs -- the
logic for tracking which instance properties are initialized exists, but the
rest of the support for the partialDeinit entry point, as well as the new
runtime function ``swift_partialDaellocInstance()`` needs to be implemented.
