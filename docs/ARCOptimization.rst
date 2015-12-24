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

RC Identity
===========

A core ARC concept in Swift optimization is the concept of ``Reference Count
Identity`` (RC Identity) and RC Identity preserving instructions. An instruction
``I`` with n SSA arguments and m SSA results is (i,j) RC Identity preserving if
performing a ``retain_value`` on the ith SSA argument immediately before ``I``
is executed is equivalent to performing a ``retain_value`` on the jth SSA result
of ``I`` immediately following the execution of ``I``. For example in the
following, if::

    retain_value %x
    %y = unary_instruction %x

is equivalent to::

    %y = unary_instruction %x
    retain_value %y

then we say that unary_instruction is a (0,0) RC Identity preserving
operation. In a case of a unary instruction, we omit (0,0) and just say that
the instruction is RC Identity preserving.

In practice generally RC Identical operations are unary operations such as
casts. This would make it seem like RC Identity is an extension of alias
analysis. But RC Identity also has significantly more power than alias analysis
since:

 - ``struct`` is an RC identity preserving operation if the ``struct`` literal
   only has one non-trivial operand. This means for instance that any struct with
   one reference counted field used as an owning pointer is RC Identical with its
   owning pointer (a useful property for Arrays).

 - An ``enum`` instruction is always RC Identical with the given tuple payload.

 - A ``tuple`` instruction is an RC identity preserving operation if the
   ``tuple`` literal has one non-trivial operand.

 - ``init_class_existential`` is an RC identity preserving operation since
   performing a retain_value on a class existential is equivalent to performing
   a retain_value on the class itself.

The corresponding value projection operations have analogous properties.

Given two SSA values ``%a``, ``%b``, we define ``%a`` as immediately RC
identical to ``%b`` if there exists an instruction ``I`` such that:

- ``%a`` is the jth result of ``I``.
- ``%b`` is the ith argument of ``I``.
- ``I`` is (i,j) RC identity preserving.

Easily the immediate RC identical relation must be reflexive and symmetric but
by its nature is not transitive. Then define the equivalence relation RC
Identity, ``~rc``, by the relations that ``%a ~rc %b`` if ``%a`` is immediately
RC identical to ``%b`` or if there is a finite sequence of n SSA values
``{%a[i]}`` such that ``%a`` is immediately RC identical to ``%a[0]`` and ``%b``
is immediately RC identical to ``%a[n]``. We currently always assume that each
equivalence class has one dominating definition.

These equivalence classes consisting of chains of RC identical values are
computed via the SILAnalysis called ``RC Identity Analysis``. By performing ARC
optimization on RC Identical operations, our optimizations are able to operate
on the level of granularity that we actually care about, ignoring superficial
changes in SSA form that still yield manipulations of the same reference count.

*NOTE* RCIdentityAnalysis is a flow insensitive analysis. Dataflow that needs to
 be flow sensitive must handle phi nodes in the dataflow itself.

*NOTE* An important consequence of RC Identity is that value types with only one
RCIdentity are a simple case for ARC optimization to handle. The ARC optimizer
relies on other optimizations like SROA, Function Signature Opts, and
SimplifyCFG (for block arguments) to try and eliminate cases where value types
have multiple reference counted subtypes.

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
    replace the reference held by that variable. The fact that
    is_unique will not actually replace it is opaque to the optimizer.

(2) If the refcount is 1 when the reference is replaced, the referent
    is deallocated.

(3) A different source-level variable pointing at the same referent
    must not be changed/invalidated by such a call.

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

To prevent removal of the apparently redundant inner retain/release
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

    - Bridged object types allow the dynamic object type check to be
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
