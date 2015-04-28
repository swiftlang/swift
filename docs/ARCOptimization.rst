:orphan:

==========================
ARC Optimization for Swift
==========================

.. contents::

.. admonition:: TODO

   This is currently a place holder for design documentation on ARC
   optimization.

Reference Counting Instructions
===============================

- strong_retain
- strong_retain_autoreleased
- strong_release
- strong_retain_unowned
- unowned_retain
- unowned_release
- load_weak
- store_weak
- fix_lifetime
- mark_dependence
- is_unique
- is_unique_or_pinned
- copy_block

Memory Behavior of ARC Operations
=================================

At SIL level, reference counting and reference checking instructions
are attributed with MayHaveSideEffects to prevent arbitrary passes
from reordering them.

At IR level, retains are marked NoModRef with respect to load and
store instructions so they don't pessimize memory dependence. (Note
the Retains are still considered to write to memory with respect to
other calls because getModRefBehavior is not overridden.) Releases
cannot be marked NoModRef because they can have arbitrary side
effects. Is_unique calls cannot be marked NoModRef because they cannot
be reordered with other operations that may modify the reference
count.

.. admonition:: TODO

   Marking runtime calls with NoModRef in LLVM is misleading (they
   write memory), inconsistent (getModRefBehavior returns Unknown),
   and fragile (e.g. if we inline ARC operations at IR level). To be
   robust and allow stronger optimization, TBAA tags should be used to
   indicate functions that only access object metadata. This would
   also enable more LLVM level optimization in the presence of
   is_unique checks which currently appear to arbitrarily write memory.

Copy-On-Write Considerations
============================

The copy-on-write capabilities of some data structures, such as Array
and Set, are efficiently implemented via Builtin.isUnique calls which
lower directly to is_unique instructions in SIL.

The is_unique instruction takes the address of a reference, and
although it does not actually change the reference, the reference must
appear mutable to the optimizer. This forces the optimizer to preserve
a retain distinct from what’s required to maintain lifetime for any of
the reference's source-level copies, because the called function is
allowed to replace the reference, thereby releasing the
referent. Consider the following sequence of rules:

(1) An operation taking the address of a variable is allowed to
    replace the reference help by that variable. The fact that
    is_unique will not actually replace it is opaque to the optimizer.

(2) If the refcount is 1 when the reference is replaced, the referent
    is be deallocated.

(3) A different source-level variable pointing at the same referent
    must not be changed/invalidated by such a call

(4) If such a variable exists, the compiler must guarantee the
    refcount is > 1 going into the call.

With the is_unique instruction, the variable whose reference is being
checked for uniqueness appears mutable at the level of an individual
SIL instruction. After IRGen, is_unique instructions are expanded into
runtime calls that no longer take the address of the
variable. Consequently, LLVM-level ARC optimization must be more
conservative. It must not remove retain/release pairs of this form:

::

   retain X
   retain X
   _swift_isUniquelyReferenced(X)
   release X
   release X

To prevent removal of the apparently redundant innder retain/release
pair, the LLVM ARC optimizer should model _swift_isUniquelyReferenced
as a function that may release X, use X, and exit the program (the
subsequent release instruction does not prove safety).

.. _arcopts.is_unique:

is_unique instruction
---------------------

As explained above, the SIL-level is_unique instruction enforces the
semantics of uniqueness checks in the presence of ARC
optimization. The kind of reference count checking that
is_unique performs depends on the argument type:

    - Native object types are directly checked by reading the strong
      reference count:
      (Builtin.NativeObject, known native class reference)

    - Objective-C object types require an additional check that the
      dynamic object type uses native swift reference counting:
      (Builtin.UnknownObject, unknown class reference, class existential)

    - Bridged object types allow the dymanic object type check to be
      bypassed based on the pointer encoding:
      (Builtin.BridgeObject)

Any of the above types may also be wrapped in an optional.  If the
static argument type is optional, then a null check is also performed.

Thus, is_unique only returns true for non-null, native swift object
references with a strong reference count of one.

is_unique_or_pinned has the same semantics as is_unique except that it
also returns true if the object is marked pinned (by strong_pin)
regardless of the reference count. This allows for simultaneous
non-structural modification of multiple subobjects.

Builtin.isUnique
----------------

Builtin.isUnique and Builtin.isUniqueOrPinned give the standard
library access to optimization safe uniqueness checking. Because the
type of reference check is derived from the builtin argument's static
type, the most efficient check is automatically generated. However, in
some cases, the standard library can dynamically determine that it has
a native reference even though the static type is a bridge or unknown
object. Unsafe variants of the builtin are available to allow the
additional pointer bit mask and dynamic class lookup to be bypassed in
these cases:

- isUnique_native : <T> (inout T[?]) -> Int1
- isUniqueOrPinned_native : <T> (inout T[?]) -> Int1

These builtins perform an implicit cast to NativeObject before
checking uniqueness. There’s no way at SIL level to cast the address
of a reference, so we need to encapsulate this operation as part of
the builtin.
