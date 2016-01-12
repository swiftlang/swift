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
a retain distinct from what's required to maintain lifetime for any of
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
checking uniqueness. There's no way at SIL level to cast the address
of a reference, so we need to encapsulate this operation as part of
the builtin.

Semantic Tags
=============

ARC takes advantage of certain semantic tags. This section documents these
semantics and their meanings.

arc.programtermination_point
----------------------------

If this semantic tag is applied to a function, then we know that:

- The function does not touch any reference counted objects.
- After the function is executed, all reference counted objects are leaked
  (most likely in preparation for program termination).

This allows one, when performing ARC code motion, to ignore blocks that contain
an apply to this function as long as the block does not have any other side
effect having instructions.

ARC Sequence Optimization
=========================

TODO: Fill this in.

ARC Loop Hoisting
=================

Abstract
--------

This section describes the ARCLoopHoisting algorithm that hoists retains and
releases out of loops. This is a high level description that justifies the
correction of the algorithm and describes its design. In the following
discussion we talk about the algorithm conceptually and show its safety and
considerations necessary for good performance.

*NOTE* In the following when we refer to "hoisting", we are not just talking
about upward code motion of retains, but also downward code motion of releases.

Loop Canonicalization
---------------------

In the following we assume that all loops are canonicalized such that:

1. The loop has a pre-header.
2. The loop has one backedge.
3. All exiting edges have a unique exit block.

Motiviation
-----------

Consider the following simple loop::

  bb0:
    br bb1

  bb1:
    retain %x                    (1)
    apply %f(%x)
    apply %f(%x)
    release %x                   (2)
    cond_br ..., bb1, bb2

  bb2:
    return ...

When it is safe to hoist (1),(2) out of the loop? Imagine if we know the trip
count of the loop is 3 and completely unroll the loop so the whole function is
one basic block. In such a case, we know the function looks as follows::

  bb0:
    # Loop Iteration 0
    retain %x
    apply %f(%x)
    apply %f(%x)
    release %x                   (4)

    # Loop Iteration 1
    retain %x                    (5)
    apply %f(%x)
    apply %f(%x)
    release %x                   (6)

    # Loop Iteration 2
    retain %x                    (7)
    apply %f(%x)
    apply %f(%x)
    release %x

    return ...

Notice how (3) can be paired with (4) and (5) can be paired with (6). Assume
that we eliminate those. Then the function looks as follows::

  bb0:
    # Loop Iteration 0
    retain %x
    apply %f(%x)
    apply %f(%x)

    # Loop Iteration 1
    apply %f(%x)
    apply %f(%x)

    # Loop Iteration 2
    apply %f(%x)
    apply %f(%x)
    release %x

    return ...

We can then re-roll the loop, yielding the following loop::

  bb0:
    retain %x                    (8)
    br bb1

  bb1:
    apply %f(%x)
    apply %f(%x)
    cond_br ..., bb1, bb2

  bb2:
    release %x                   (9)
    return ...

Notice that this transformation is equivalent to just hoisting (1) and (2) out
of the loop in the original example. This form of hoisting is what is termed
"ARCLoopHoisting". What is key to notice is that even though we are performing
"hoisting" we are actually pairing releases from one iteration with retains in
the next iteration and then eliminating the pairs. This realization will guide
our further analysis.

Correctness
-----------

In this simple loop case, the proof of correctness is very simple to see
conceptually. But in a more general case, when is safe to perform this
optimization? We must consider three areas of concern:

1. Are the retains/releases upon the same reference count? This can be found
   conservatively by using RCIdentityAnalysis.

2. Can we move retains, releases in the unrolled case as we have specified?
   This is simple since it is always safe to move a retain earlier and a release
   later in the dynamic execution of a program. This can only extend the life of
   a variable which is a legal and generally profitable in terms of allowing for
   this optimization.

3. How do we pair all necessary retains/releases to ensure we do not unbalance
   retain/release counts in the loop? Consider a set of retains and a set of
   releases that we wish to hoist out of a loop. We can only hoist the retain,
   release sets out of the loop if all paths in the given loop region from the
   entrance to the backedge.  have exactly one retain or release from this set.

4. Any early exits that we must move a retain past or a release by must be
   compensated appropriately. This will be discussed in the next section.

Assuming that our optimization does all of these things, we should be able to
hoist with safety.

Compensating Early Exits for Lost Dynamic Reference Counts
----------------------------------------------------------

Lets say that we have the following loop canonicalized SIL::

  bb0(%0 : $Builtin.NativeObject):
    br bb1

  bb1:
    strong_retain %0 : $Builtin.NativeObject
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject
    cond_br ..., bb2, bb3

  bb2:
    cond_br ..., bb1, bb4

  bb3:
    br bb5

  bb4:
    br bb5

  bb6:
    return ...

Can we hoist the retain/release pair here? Lets assume the loop is 3 iterations
and we completely unroll it. Then we have::

  bb0:
    strong_retain %0 : $Builtin.NativeObject               (1)
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject              (2)
    cond_br ..., bb1, bb4

  bb1: // preds: bb0
    strong_retain %0 : $Builtin.NativeObject               (3)
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject              (4)
    cond_br ..., bb2, bb4

  bb2: // preds: bb1
    strong_retain %0 : $Builtin.NativeObject               (5)
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject              (6)
    cond_br ..., bb3, bb4

  bb3: // preds: bb2
    br bb5

  bb4: // preds: bb0, bb1, bb2
    br bb5

  bb5: // preds: bb3, bb4
    return ...

We want to be able to pair and eliminate (2)/(3) and (4)/(5). In order to do
that, we need to move (2) from bb0 into bb1 and (4) from bb1 into bb2. In order
to do this, we need to move a release along all paths into bb4 lest we lose
dynamic releases along that path. We also sink (6) in order to not have an extra
release along that path. This then give us::

  bb0:
    strong_retain %0 : $Builtin.NativeObject               (1)

  bb1:
    apply %f(%0)
    apply %f(%0)
    cond_br ..., bb2, bb3

  bb2:
    cond_br ..., bb1, bb4

  bb3:
    strong_release %0 : $Builtin.NativeObject              (6*)
    br bb5

  bb4:
    strong_release %0 : $Builtin.NativeObject              (7*)
    br bb5

  bb5: // preds: bb3, bb4
    return ...

An easy inductive proof follows.

What if we have the opposite problem, that of moving a retain past an early
exit. Consider the following::

  bb0(%0 : $Builtin.NativeObject):
    br bb1

  bb1:
    cond_br ..., bb2, bb3

  bb2:
    strong_retain %0 : $Builtin.NativeObject
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject
    cond_br ..., bb1, bb4

  bb3:
    br bb5

  bb4:
    br bb5

  bb6:
    return ...

Lets unroll this loop::

  bb0(%0 : $Builtin.NativeObject):
    br bb1

  # Iteration 1
  bb1: // preds: bb0
    cond_br ..., bb2, bb8

  bb2: // preds: bb1
    strong_retain %0 : $Builtin.NativeObject               (1)
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject              (2)
    br bb3

  # Iteration 2
  bb3: // preds: bb2
    cond_br ..., bb4, bb8

  bb4: // preds: bb3
    strong_retain %0 : $Builtin.NativeObject               (3)
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject              (4)
    br bb5

  # Iteration 3
  bb5: // preds: bb4
    cond_br ..., bb6, bb8

  bb6: // preds: bb5
    strong_retain %0 : $Builtin.NativeObject               (5)
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject              (6)
    cond_br ..., bb7, bb8

  bb7: // preds: bb6
    br bb9

  bb8: // Preds: bb1, bb3, bb5, bb6
    br bb9

  bb9:
    return ...

First we want to move the retain into the previous iteration. This means that we
have to move a retain over the cond_br in bb1, bb3, bb5. If we were to do that
then bb8 would have an extra dynamic retain along that path. In order to fix
that issue, we need to balance that release by putting a release in bb8. But we
cannot move a release into bb8 without considering the terminator of bb6 since
bb6 is also a predecessor of bb8. Luckily, we have (6). Notice that bb7 has one
predecessor to bb6 so we can safely move 1 release along that path as well. Thus
we perform that code motion, yielding the following::

  bb0(%0 : $Builtin.NativeObject):
    br bb1

  # Iteration 1
  bb1: // preds: bb0
    strong_retain %0 : $Builtin.NativeObject               (1)
    cond_br ..., bb2, bb8

  bb2: // preds: bb1
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject              (2)
    br bb3

  # Iteration 2
  bb3: // preds: bb2
    strong_retain %0 : $Builtin.NativeObject               (3)
    cond_br ..., bb4, bb8

  bb4: // preds: bb3
    apply %f(%0)
    apply %f(%0)
    strong_release %0 : $Builtin.NativeObject              (4)
    br bb5

  # Iteration 3
  bb5: // preds: bb4
    strong_retain %0 : $Builtin.NativeObject               (5)
    cond_br ..., bb6, bb8

  bb6: // preds: bb5
    apply %f(%0)
    apply %f(%0)
    cond_br ..., bb7, bb8

  bb7: // preds: bb6
    strong_release %0 : $Builtin.NativeObject              (7*)
    br bb9

  bb8: // Preds: bb1, bb3, bb5, bb6
    strong_release %0 : $Builtin.NativeObject              (8*)
    br bb9

  bb9:
    return ...

Then we move (1), (3), (4) into the single predecessor of their parent block and
eliminate (3), (5) through a pairing with (2), (4) respectively. This yields
then::

  bb0(%0 : $Builtin.NativeObject):
    strong_retain %0 : $Builtin.NativeObject               (1)
    br bb1

  # Iteration 1
  bb1: // preds: bb0
    cond_br ..., bb2, bb8

  bb2: // preds: bb1
    apply %f(%0)
    apply %f(%0)
    br bb3

  # Iteration 2
  bb3: // preds: bb2
    cond_br ..., bb4, bb8

  bb4: // preds: bb3
    apply %f(%0)
    apply %f(%0)
    br bb5

  # Iteration 3
  bb5: // preds: bb4
    cond_br ..., bb6, bb8

  bb6: // preds: bb5
    apply %f(%0)
    apply %f(%0)
    cond_br ..., bb7, bb8

  bb7: // preds: bb6
    strong_release %0 : $Builtin.NativeObject              (7*)
    br bb9

  bb8: // Preds: bb1, bb3, bb5, bb6
    strong_release %0 : $Builtin.NativeObject              (8*)
    br bb9

  bb9:
    return ...

Then we finish by rerolling the loop::

  bb0(%0 : $Builtin.NativeObject):
    strong_retain %0 : $Builtin.NativeObject               (1)
    br bb1

  # Iteration 1
  bb1: // preds: bb0
    cond_br ..., bb2, bb8

  bb2:
    apply %f(%0)
    apply %f(%0)
    cond_br bb1, bb7

  bb7:
    strong_release %0 : $Builtin.NativeObject              (7*)
    br bb9

  bb8: // Preds: bb1, bb3, bb5, bb6
    strong_release %0 : $Builtin.NativeObject              (8*)
    br bb9

  bb9:
    return ...


Uniqueness Check Complications
------------------------------

A final concern that we must consider is if we introduce extra copy on write
copies through our optimization. To see this, consider the following simple
IR sequence::

  bb0(%0 : $Builtin.NativeObject):
    // refcount(%0) == n
    is_unique %0 : $Builtin.NativeObject
    // refcount(%0) == n
    strong_retain %0 : $Builtin.NativeObject
    // refcount(%0) == n+1

If n is not 1, then trivially is_unique will return false. So assume that n is 1
for our purposes so no copy is occurring here. Thus we have::

  bb0(%0 : $Builtin.NativeObject):
    // refcount(%0) == 1
    is_unique %0 : $Builtin.NativeObject
    // refcount(%0) == 1
    strong_retain %0 : $Builtin.NativeObject
    // refcount(%0) == 2

Now imagine that we move the strong_retain before the is_unique. Then we have::

  bb0(%0 : $Builtin.NativeObject):
    // refcount(%0) == 1
    strong_retain %0 : $Builtin.NativeObject
    // refcount(%0) == 2
    is_unique %0 : $Builtin.NativeObject

Thus is_unique is guaranteed to return false introducing a copy that was not
needed. We wish to avoid that if it is at all possible.

