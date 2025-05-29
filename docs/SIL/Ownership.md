# Ownership SSA

This document describes Ownership SSA in detail. For an overview of SIL and
OSSA see the [SIL](SIL.md) document.

## Overview

A function marked with the `[ossa]` function attribute is considered
to be in Ownership SSA form. Ownership SSA is an augmented version of
SSA that enforces ownership invariants by imbuing value-operand edges
with semantic ownership information. All SIL values are assigned a
constant ownership kind that defines the ownership semantics that the
value models. All SIL operands that use a SIL value are required to be
able to be semantically partitioned in between "non-lifetime ending
uses" that just require the value to be live and "lifetime ending
uses" that end the lifetime of the value and after which the value can
no longer be used. Since by definition operands that are lifetime ending
uses end their associated value's lifetime, we must have that, ignoring
program ending [Dead End Blocks](#dead-end-blocks), the lifetime ending
use points jointly post-dominate all non-lifetime ending use points and
that a value must have exactly one lifetime ending use along all
reachable program paths, preventing leaks and use-after-frees. As an
example, consider the following SIL example with partitioned defs/uses
annotated inline:

```
sil @stash_and_cast : $@convention(thin) (@owned Klass) -> @owned SuperKlass {
bb0(%kls1 : @owned $Klass): // Definition of %kls1

  // "Normal Use" kls1.
  // Definition of %kls2.
  %kls2 = copy_value %kls1 : $Klass

  // "Consuming Use" of %kls2 to store it into a global. Stores in ossa are
  // consuming since memory is generally assumed to have "owned"
  // semantics. After this instruction executes, we can no longer use %kls2
  // without triggering an ownership violation.
  store %kls2 to [init] %globalMem : $*Klass

  // "Consuming Use" of %kls1.
  // Definition of %kls1Casted.
  %kls1Casted = upcast %kls1 : $Klass to $SuperKlass

  // "Consuming Use" of %kls1Casted
  return %kls1Casted : $SuperKlass
}
```

Notice how every value in the SIL above has a partition-able set of uses
with normal uses always before consuming uses. Any such violations of
ownership semantics would trigger a SILVerifier error allowing us to
know that we do not have any leaks or use-after-frees in the above code.

## Ownership Kind

The semantics in the previous example is of just one form of ownership
semantics supported: "owned" semantics. In SIL, we map these
"ownership semantics" into a form that a compiler can reason about by
mapping semantics onto a lattice with the following elements:
`None`, `Owned`, `Guaranteed`,
`Unowned`, `[Any]`. We call this the lattice of
"Ownership Kinds" and each individual value an "Ownership Kind".
This lattice is defined as a 3-level lattice with:

```
1. None being Top.
2. Any being Bottom.
3. All non-Any, non-None OwnershipKinds being defined as a mid-level elements of the lattice
```

We can graphically represent the lattice via a diagram like the
following:

```
          +------+
+-------- | None | ---------+
|         +------+          |
|            |              |
v            v              v             ^
+-------+  +-----+------+  +---------+    |
| Owned |  | Guaranteed |  | Unowned |    +--- Value Ownership Kinds and
+-------+  +-----+------+  +---------+         Ownership Constraints
|            |              |
|            v              |         +--- Only Ownership Constraints
|         +-----+           |         |
+-------->| Any |<----------+         v
          +-----+
```

One moves down the lattice by performing a "meet" operation:

```
None meet OtherOwnershipKind -> OtherOwnershipKind
Unowned meet Owned -> Any
Owned meet Guaranteed -> Any
```

and one moves up the lattice by performing a "join" operation, e.x.:

```
Any join OtherOwnershipKind -> OtherOwnershipKind
Owned join Any -> Owned
Owned join Guaranteed -> None
```

This lattice is applied to SIL by requiring well formed SIL to:

1.  Define a map of each SIL value to a constant OwnershipKind that
    classify the semantics that the SIL value obeys. This ownership kind
    may be static (i.e.: the same for all instances of an instruction)
    or dynamic (e.x.: forwarding instructions set their ownership upon
    construction). We call this subset of OwnershipKind to be the set of
    [Value Ownership Kind](#value-ownership-kind): [None](#none),
    [Unowned](#unowned), [Guaranteed](#guaranteed), [Owned](#owned)
    (note conspicuously missing `Any`). This is because in
    our model `Any` represents an unknown ownership
    semantics and since our model is strict, we do not allow for values
    to have unknown ownership.
    
2.  Define a map from each operand of a SILInstruction, `i`,
    to a constant Ownership Kind, Boolean pair called the operand's
    [Ownership Constraint](#ownership-constraint). The Ownership Kind
    element of the [Ownership Constraint](#ownership-constraint)
    determines semantically which ownership kind's the operand's value
    can take on. The Boolean value is used to know if an operand will
    end the lifetime of the incoming value when checking dataflow rules.
    The dataflow rules that each [Value Ownership
    Kind](#value-ownership-kind) obeys is documented for each [Value
    Ownership Kind](#value-ownership-kind) in its detailed description
    below.

Then we take these two maps and require that valid SIL has the property
that given an operand, `op(i)` of an instruction `i` and a value `v`
that `op(i)` can only use `v` if the `join` of
`OwnershipConstraint(operand(i))` with `ValueOwnershipKind(v)` is equal
to the `ValueOwnershipKind` of `v`. In symbols, we must have that:

```
join : (OwnershipConstraint, ValueOwnershipKind) -> ValueOwnershipKind
OwnershipConstraint(operand(i)) join ValueOwnershipKind(v) = ValueOwnershipKind(v)
```

In words, a value can be passed to an operand if applying the operand's
ownership constraint to the value's ownership does not change the
value's ownership. Operationally this has a few interesting effects on
SIL:

1.  We have defined away invalid value-operand (aka def-use) pairing
    since the SILVerifier validates the aforementioned relationship on
    all SIL values, uses at all points of the pipeline until OSSA is
    lowered.
2.  Many SIL instructions do not care about the ownership kind that
    their value will take. They can just define all of their operand's
    as having an ownership constraint of Any.

Now lets go into more depth upon [Value Ownership
Kind](#value-ownership-kind) and [Ownership
Constraint](#ownership-constraint).

## Value Ownership Kind

As mentioned above, each SIL value is statically mapped to an [Ownership
Kind](#ownership-kind) called the value's "ValueOwnershipKind" that
classify the semantics of the value. Below, we map each
ValueOwnershipKind to a short summary of the semantics implied upon the
parent value:

-   **None**. This is used to represent values that do not require
    memory management and are outside of Ownership SSA invariants.
    Examples: trivial values (e.x.: Int, Float), non-payloaded cases of
    non-trivial enums (e.x.: `Optional<T>.none`), all address types.
-   **Owned**. A value that exists independently of any other value and
    is consumed exactly once along all paths through a function by
    either a destroy_value (actually destroying the value) or by a
    consuming instruction that rebinds the value in some manner (e.x.:
    apply, casts, store).
-   **Guaranteed**. A value with a scoped lifetime whose liveness is
    dependent on the lifetime of some other "enclosing" owned or guaranteed
    value. Consumed by instructions like [`end_borrow`](Instructions.md#end_borrow). The
    "enclosing" value is statically guaranteed to be live at all of the
    value's paired end_borrow instructions.
-   **Unowned**. A value that is only guaranteed to be instantaneously
    valid and must be copied before the value is used in an `@owned` or
    `@guaranteed` context. This is needed both to model argument values
    with the ObjC unsafe unowned argument convention and also to model
    the ownership resulting from bitcasting a trivial type to a
    non-trivial type. This value should never be consumed.

We describe each of these semantics in below in more detail.

### Owned

Owned ownership models "move only" values. We require that each such
value is consumed exactly once along all program paths. The IR verifier
will flag values that are not consumed along a path as a leak and any
double consumes as use-after-frees. We model move operations via
[forwarding uses](#forwarding-uses) such as casts and transforming
terminators (e.x.: [switch_enum](Instructions.md#switch_enum),
[checked_cast_br](Instructions.md#checked_cast_br)) that transform the input value,
consuming it in the process, and producing a new transformed owned value
as a result.

Putting this all together, one can view each owned SIL value as being
effectively a "move only value" except when explicitly copied by a
copy_value. This of course implies that ARC operations can be assumed to
only semantically effect the specific value that they are applied to
/and/ that each ARC constraint is able to be verified independently for
each owned SILValue derived from the ARC object. As an example, consider
the following Swift/SIL:

```
// testcase.swift.
func doSomething(x : Klass) -> OtherKlass? {
  return x as? OtherKlass
}

// testcase.sil. A possible SILGen lowering
sil [ossa] @doSomething : $@convention(thin) (@guaranteed Klass) -> () {
bb0(%0 : @guaranteed Klass):
  // Definition of '%1'
  %1 = copy_value %0 : $Klass

  // Consume '%1'. This means '%1' can no longer be used after this point. We
  // rebind '%1' in the destination blocks (bbYes, bbNo).
  checked_cast_br Klass in %1 : $Klass to $OtherKlass, bbYes, bbNo

bbYes(%2 : @owned $OtherKlass): // On success, the checked_cast_br forwards
                                // '%1' into '%2' after casting to OtherKlass.

  // Forward '%2' into '%3'. '%2' can not be used past this point in the
  // function.
  %3 = enum $Optional<OtherKlass>, case #Optional.some!enumelt, %2 : $OtherKlass

  // Forward '%3' into the branch. '%3' can not be used past this point.
  br bbEpilog(%3 : $Optional<OtherKlass>)

bbNo(%3 : @owned $Klass): // On failure, since we consumed '%1' already, we
                          // return the original '%1' as a new value '%3'
                          // so we can use it below.
  // Actually destroy the underlying copy (``%1``) created by the copy_value
  // in bb0.
  destroy_value %3 : $Klass

  // We want to return nil here. So we create a new non-payloaded enum and
  // pass it off to bbEpilog.
  %4 = enum $Optional<OtherKlass>, case #Optional.none!enumelt
  br bbEpilog(%4 : $Optional<OtherKlass>)

bbEpilog(%5 : @owned $Optional<OtherKlass>):
  // Consumes '%5' to return to caller.
  return %5 : $Optional<OtherKlass>
}
```

Notice how our individual copy (`%1`) threads its way through the IR
using [forwarding uses](#forwarding-uses) of `@owned` ownership. These
[forwarding uses](#forwarding-uses) partition the lifetime of the result
of the copy_value into a set of disjoint individual owned lifetimes
(`%2`, `%3`, `%5`).

### Guaranteed

Guaranteed ownership models values that have a scoped dependent lifetime
on a "base value" with owned or guaranteed ownership. Due to this
lifetime dependence, the base value is required to be statically live
over the entire scope where the guaranteed value is valid.

These explicit scopes are introduced into SIL by begin scope
instructions (e.x.: [begin_borrow](Instructions.md#begin_borrow),
[load_borrow](Instructions.md#load_borrow)) that are paired with sets of jointly
post-dominating scope ending instructions (e.x.:
[end_borrow](Instructions.md#end_borrow)):

```
sil [ossa] @guaranteed_values : $@convention(thin) (@owned Klass) -> () {
bb0(%0 : @owned $Klass):
  %1 = begin_borrow %0 : $Klass
  cond_br ..., bb1, bb2

bb1:
  ...
  end_borrow %1 : $Klass
  destroy_value %0 : $Klass
  br bb3

bb2:
  ...
  end_borrow %1 : $Klass
  destroy_value %0 : $Klass
  br bb3

bb3:
  ...
}
```

Notice how the [end_borrow](Instructions.md#end_borrow) allow for a SIL generator to
communicate to optimizations that they can never shrink the lifetime of
`%0` by moving [destroy_value](Instructions.md#destroy_value) above `%1`.

Values with guaranteed ownership follow a dataflow rule that states that
non-consuming [forwarding uses](#forwarding-uses) of the guaranteed
value are also guaranteed and are recursively validated as being in the
original values scope. This was a choice we made to reduce idempotent
scopes in the IR:

```
sil [ossa] @get_first_elt : $@convention(thin) (@guaranteed (String, String)) -> @owned String {
bb0(%0 : @guaranteed $(String, String)):
  // %1 is validated as if it was apart of %0 and does not need its own begin_borrow/end_borrow.
  %1 = tuple_extract %0 : $(String, String)
  // So this copy_value is treated as a use of %0.
  %2 = copy_value %1 : $String
  return %2 : $String
}
```

### None

Values with None ownership are inert values that exist outside of the
guarantees of Ownership SSA. Some examples of such values are:

-   Trivially typed values such as: Int, Float, Double
-   Non-payloaded non-trivial enums.
-   Address types.

Since values with none ownership exist outside of ownership SSA, they
can be used like normal SSA without violating ownership SSA invariants.
This does not mean that code does not potentially violate other SIL
rules (consider memory lifetime invariants):

```
sil @none_values : $@convention(thin) (Int, @in Klass) -> Int {
bb0(%0 : $Int, %1 : $*Klass):

  // %0, %1 are normal SSA values that can be used anywhere in the function
  // without breaking Ownership SSA invariants. It could violate other
  // invariants if for instance, we load from %1 after we destroy the object
  // there.
  destroy_addr %1 : $*Klass

  // If uncommented, this would violate memory lifetime invariants due to
  // the ``destroy_addr %1`` above. But this would not violate the rules of
  // Ownership SSA since addresses exist outside of the guarantees of
  // Ownership SSA.
  //
  // %2 = load [take] %1 : $*Klass

  // I can return this object without worrying about needing to copy since
  // none objects can be arbitrarily returned.
  return %0 : $Int
}
```

### Unowned

This is a form of ownership that is used to model two different use
cases:

-   Arguments of functions with ObjC convention. This convention
    requires the callee to copy the value before using it (preferably
    before any other code runs). We do not model this flow sensitive
    property in SIL today, but we do not allow for unowned values to be
    passed as owned or guaranteed values without copying it first.
-   Values that are a conversion from a trivial value with None
    ownership to a non-trivial value. As an example of this consider an
    unsafe bit cast of a trivial pointer to a class. In that case, since
    we have no reason to assume that the object will remain alive, we
    need to make a copy of the value.

## Ownership Constraint

NOTE: We assume that one has read the section above on [Ownership
Kind](#ownership-kind).

As mentioned above, every operand `operand(i)` of a SIL instruction `i`
has statically mapped to it:

1.  An ownership kind that acts as an "Ownership Constraint" upon what
    "Ownership Kind" a value can take.
2.  A boolean value that defines whether or not the execution of the
    operand's instruction will cause the operand's value to be
    invalidated. This is often times referred to as an operand acting as
    a "lifetime ending use".

## Forwarding Uses

NOTE: In the following, we assumed that one read the section above,
[Ownership Kind](#ownership-kind), [Value Ownership
Kind](#value-ownership-kind) and [Ownership
Constraint](#ownership-constraint).

A subset of SIL instructions define the value ownership kind of their
results in terms of the value ownership kind of their operands. Such an
instruction is called a "forwarding instruction" and any use with such
a user instruction a "forwarding use". This inference generally occurs
upon instruction construction and as a result:

-   When manipulating forwarding instructions programmatically, one must
    manually update their forwarded ownership since most of the time the
    ownership is stored in the instruction itself. Don't worry though
    because the SIL verifier will catch this error for you if you forget
    to do so!

-   Textual SIL does not represent the ownership of forwarding
    instructions explicitly. Instead, the instruction's ownership is
    inferred normally from the parsed operand. In some cases the
    forwarding ownership kind is different from the ownership kind of
    its operand. In such cases, textual SIL represents the forwarding
    ownership kind explicitly. Eg: :

    ```
    %cast = unchecked_ref_cast %val : $Klass to $Optional<Klass>, forwarding: @unowned
    ```

    Since the SILVerifier runs on Textual SIL after parsing, you can
    feel confident that ownership constraints were inferred correctly.

Forwarding has slightly different ownership semantics depending on the
value ownership kind of the operand on construction and the result's
type. We go through each below:

-   Given an `@owned` operand, the forwarding instruction is assumed to
    end the lifetime of the operand and produce an `@owned` value if
    non-trivially typed and `@none` if trivially typed. Example: This is
    used to represent the semantics of casts:

    ```
    sil @unsafelyCastToSubClass : $@convention(thin) (@owned Klass) -> @owned SubKlass {
    bb0(%0 : @owned $Klass): // %0 is defined here.

      // %0 is consumed here and can no longer be used after this point.
      // %1 is defined here and after this point must be used to access the object
      // passed in via %0.
      %1 = unchecked_ref_cast %0 : $Klass to $SubKlass

      // Then %1's lifetime ends here and we return the casted argument to our
      // caller as an @owned result.
      return %1 : $SubKlass
    }
    ```

-   Given a `@guaranteed` operand, the forwarding instruction is assumed
    to produce `@guaranteed` non-trivially typed values and `@none`
    trivially typed values. Given the non-trivial case, the instruction
    is assumed to begin a new implicit borrow scope for the incoming
    value. Since the borrow scope is implicit, we validate the uses of
    the result as if they were uses of the operand (recursively). This
    of course means that one should never see end_borrows on any
    guaranteed forwarded results, the end_borrow is always on the
    instruction that "introduces" the borrowed value. An example of a
    guaranteed forwarding instruction is `struct_extract`:

    ```
    // In this function, I have a pair of Klasses and I want to grab some state
    // and then call the hand off function for someone else to continue
    // processing the pair.
    sil @accessLHSStateAndHandOff : $@convention(thin) (@owned KlassPair) -> @owned State {
    bb0(%0 : @owned $KlassPair): // %0 is defined here.

      // Begin the borrow scope for %0. We want to access %1's subfield in a
      // read only way that doesn't involve destructuring and extra copies. So
      // we construct a guaranteed scope here so we can safely use a
      // struct_extract.
      %1 = begin_borrow %0 : $KlassPair

      // Now we perform our struct_extract operation. This operation
      // structurally grabs a value out of a struct without safety relying on
      // the guaranteed ownership of its operand to know that %1 is live at all
      // use points of %2, its result.
      %2 = struct_extract %1 : $KlassPair, #KlassPair.lhs

      // Then grab the state from our left hand side klass and copy it so we
      // can pass off our klass pair to handOff for continued processing.
      %3 = ref_element_addr %2 : $Klass, #Klass.state
      %4 = load [copy] %3 : $*State

      // Now that we have finished accessing %1, we end the borrow scope for %1.
      end_borrow %1 : $KlassPair

      %handOff = function_ref @handOff : $@convention(thin) (@owned KlassPair) -> ()
      apply %handOff(%0) : $@convention(thin) (@owned KlassPair) -> ()

      return %4 : $State
    }
    ```

-   Given an `@none` operand, the result value must have `@none`
    ownership.

-   Given an `@unowned` operand, the result value will have `@unowned`
    ownership. It will be validated just like any other `@unowned`
    value, namely that it must be copied before use.

An additional wrinkle here is that even though the vast majority of
forwarding instructions forward all types of ownership, this is not true
in general. To see why this is necessary, lets compare/contrast
[struct_extract](Instructions.md#struct_extract) (which does not forward `@owned`
ownership) and [unchecked_enum_data](Instructions.md#unchecked_enum_data) (which can
forward /all/ ownership kinds). The reason for this difference is that
[struct_extract](Instructions.md#struct_extract) inherently can only extract out a
single field of a larger object implying that the instruction could only
represent consuming a sub-field of a value instead of the entire value
at once. This violates our constraint that owned values can never be
partially consumed: a value is either completely alive or completely
dead. In contrast, enums always represent their payloads as elements in
a single tuple value. This means that
[unchecked_enum_data](Instructions.md#unchecked_enum_data) when it extracts that
payload from an enum, can consume the entire enum+payload.

To handle cases where we want to use [struct_extract](Instructions.md#struct_extract)
in a consuming way, we instead are able to use the
[destructure_struct](Instructions.md#destructure_struct) instruction that consumes the
entire struct at once and gives one back the structs individual
constituent parts:

```
struct KlassPair {
  var fieldOne: Klass
  var fieldTwo: Klass
}

sil @getFirstPairElt : $@convention(thin) (@owned KlassPair) -> @owned Klass {
bb0(%0 : @owned $KlassPair):
  // If we were to just do this directly and consume KlassPair to access
  // fieldOne... what would happen to fieldTwo? Would it be consumed?
  //
  // %1 = struct_extract %0 : $KlassPair, #KlassPair.fieldOne
  //
  // Instead we need to destructure to ensure we consume the entire owned value at once.
  (%1, %2) = destructure_struct $KlassPair

  // We only want to return %1, so we need to cleanup %2.
  destroy_value %2 : $Klass

  // Then return %1 to our caller
  return %1 : $Klass
}
```

## Forwarding Address-Only Values

Address-only values are potentially unmovable when borrowed. This means
that they cannot be forwarded with guaranteed ownership unless the
forwarded value has the same representation as in the original value and
can reuse the same storage. Non-destructive projection is allowed, such
as `struct_extract`. Aggregation, such as
`struct`, and destructive disaggregation, such as
`switch_enum` is not allowed. This is an invariant for OSSA
with opaque SIL values for these reasons:

1. To avoid implicit semantic copies. For move-only values, this allows
complete diagnostics. And in general, it makes it impossible for SIL
passes to "accidentally" create copies.

2. To reuse borrowed storage. This allows the optimizer to share the
same storage for multiple exclusive reads of the same variable, avoiding
copies. It may also be necessary to support native Swift atomics, which
will be unmovable-when-borrowed.

## Borrowed Object based Safe Interior Pointers

### What is an "Unsafe Interior Pointer"

An unsafe interior pointer is a bare pointer into the innards of an
object. A simple example of this in C++ would be using the method
`std::vector::data()` to get to the innards of a `std::vector`. In general
interior pointers are unsafe to use since languages do not provide any
guarantees that the interior pointer will not be used after the
underlying object has been deallocated. To see this, consider the
following C++ example:

```
int unfortunateFunction() {
  int *unsafeInteriorPointer = nullptr;
  {
    std::vector<int> vector;
    vector.push_back(5);
    unsafeInteriorPointer = vector.data();
    printf("%dn", *unsafeInteriorPointer); // Prints "5".
  } // vector deallocated here
  return *unsafeInteriorPointer; // Kaboom
}
```

In words, C++ allows for us to get the interior pointer into the vector,
but then lets us do whatever we want with the pointer, including use it
after the underlying memory has been invalidated.

From a user's perspective, interior pointers are really useful since
one can use it to pass data to other APIs that are only expecting a
pointer and also since one can use it to sometimes get better
performance. But from a language designer perspective, this sort of API
verboten and leads to bugs, crashes, and security vulnerabilities. That
being said, clearly users have a need for such functionality, so we, as
language designers, should figure out manners to express these sorts of
patterns in our various languages in a safe way that prevents user's
from foot-gunning themselves. In SIL, we have solved this problem via
the direct modeling of interior pointer instructions as a high level
concept in our IR.

### Safe Interior Pointers in SIL

In contrast to LLVM-IR, SIL provides mechanisms that language designers
can use to express concepts like the above in a manner that allows the
language to define away compiler generated unsafe interior pointer usage
using "Safe Interior Pointers". This is implemented in SIL by:

1.  Classifying a set of instructions as being "interior pointer"
    instructions.
2.  Enforcing in the SILVerifier that all "interior pointer"
    instructions can only have operands with [Guaranteed](#guaranteed)
    ownership.
3.  Enforcing in the SILVerifier that any transitive address use of the
    interior pointer to be a liveness requirement of the "interior
    pointer"'s operand.

Note that the transitive address use verifier from (3) does not attempt
to classify uses directly. Instead the verifier:

1.  Has an explicit list of instructions that it understands as
    requiring liveness of the base object.
2.  Has a second list of instructions that require liveness and produce
    a address whose transitive uses need to be recursively processed.
3.  Asserts on any instructions that are not known to the verifier. This
    ensures that the verifier is kept up to date with new instructions.

Note that typically instructions in category (1) are instructions whose
uses do not propagate the pointer value, so they are safe. In contrast,
some other instructions in category (1) are escaping uses of the address
such as [pointer_to_address](Instructions.md#pointer_to_address). Those uses are
unsafe--the user is responsible for managing unsafe pointer lifetimes
and the compiler must not extend those pointer lifetimes.

These rules ensure statically that any uses of the address that are not
escaped explicitly by an instruction like
[pointer_to_address](Instructions.md#pointer_to_address) are within the guaranteed
pointers scope where the guaranteed value is statically known to be
live. As a result, in SIL it is impossible to express such a bug in
compiler generated code. As an example, consider the following unsafe
interior pointer SIL:

```
class Klass { var k: KlassField }
struct KlassWrapper { var k: Klass }

// ...

// Today SIL restricts interior pointer instructions to only have operands
// with guaranteed ownership.
%1 = begin_borrow %0 : $Klass

// %2 is an interior pointer into %1. Since %2 is an address, it's uses are
// not treated as uses of underlying borrowed object %1 in the ownership
// system. This is because at the ownership level objects with None
// ownership are not verified and do not have any constraints on how they
// are used from the ownership system.
//
// Instead the ownership verifier gathers up all such uses and treats them
// as uses of the object from which the interior pointer was projected from
// transitively. This means that this is a constraint on the guaranteed
// objects use, not on the trivial values.
%2 = ref_element_addr %1 : $Klass, #Klass.k // %2 is a $*KlassWrapper
%3 = struct_element_addr %2 : $*KlassWrapper, #KlassWrapper.k // %3 is a $*Klass

// So if we end the borrow %1 at this point, invalidating the addresses
// ``%2`` and ``%3``.
end_borrow %1 : $Klass

// We would here be loading from an invalidated address. This would cause a
// verifier error since %3's use here is a regular use that is inferred up
// on %1.
%4 = load [copy] %3 : $*KlassWrapper

// ...
```

Notice how due to a possible bug in the compiler, we are loading from
potentially uninitialized memory `%4`. This would have caused a verifier
error stating that `%4` was an interior pointer based use-after-free of
`%1` implying this is mal-formed SIL.

NOTE: This is a constraint on the base object, not on the addresses
themselves which are viewed as outside of the ownership system since
they have [None](#none) ownership.

In contrast to the previous example, the following example follows
ownership invariants and is valid SIL:

```
class Klass { var k: KlassField }
struct KlassWrapper { var k: Klass }

// ...

%1 = begin_borrow %0 : $Klass
// %2 is an interior pointer into the Klass k. Since %2 is an address and
// addresses have None ownership, it's uses are not treated as uses of the
// underlying object %1.
%2 = ref_element_addr %1 : $Klass, #Klass.k // %2 is a $*KlassWrapper

// Destroying %1 at this location would result in a verifier error since
// %2's uses are considered to be uses of %1.
//
// end_lifetime %1 : $Klass

// We are statically not loading from an invalidated address here since we
// are within the lifetime of ``%1``.
%3 = struct_element_addr %2 : $*KlassWrapper, #KlassWrapper.k
%4 = load [copy] %3 : $*Klass // %1 must be live here transitively

// ``%1``'s lifetime ends. Importantly we know that within the lifetime of
// ``%1``, ``%0``'s lifetime can not shrink past this point, implying
// transitive static safety.
end_borrow %1 : $Klass
```

In the second example, we show a well-formed SIL program showing off
SIL's Safe Interior Pointers. All of the uses of `%2`, the interior
pointer, are transitively uses of the base underlying object, `%0`.

The current list of interior pointer SIL instructions are:

-   [project_box](Instructions.md#project_box) - projects a pointer out of a reference
    counted box. (*)
-   [ref_element_addr](Instructions.md#ref_element_addr) - projects a field out of a
    reference counted class.
-   [ref_tail_addr](Instructions.md#ref_tail_addr) - projects out a pointer to a
    class's tail allocated array memory (assuming the class was
    initialized to have such an array).
-   [open_existential_box](Instructions.md#open_existential_box) - projects the address
    of the value out of a boxed existential container using the current
    function context/protocol conformance to create an "opened
    archetype".
-   [project_existential_box](Instructions.md#project_existential_box) - projects a
    pointer to the value inside a boxed existential container. Must be
    the type for which the box was initially allocated for and not for
    an "opened" archetype.

(*) We still need to finish adding support for project_box, but all
other interior pointers are guarded already.

## Variable Lifetimes

In order for programmer intended lifetimes to be maintained under
optimization, the lifetimes of SIL values which correspond to named
source-level values can only be modified in limited ways. Generally, the
behavior is that the lifetime of a named source-level value is anchored
to the variable's lexical scope and confined by **deinit barriers**.
Specifically, code motion may not move the ends of these lifetimes
across a deinit barrier.

Source level variables (lets, vars, ...) and function arguments will
result in SIL-level lexical lifetimes if either of the two sets of
circumstances apply:

* Inferred lexicality.
    -   the type is non-trivial
    -   the type is not eager-move
    -   the variable or argument is not annotated to be eager-move

OR

* Explicit lexicality.
    -   the type, variable, or argument is annotated
        `@_lexical`

A type is eager-move by satisfying one of two conditions:

1. Inferred: An aggregate is inferred to be eager-move if all of its
fields are eager-move.

1. Annotated: Any type can be eager-move if it is annotated with an
attribute that explicitly specifies it to be:
`@_eagerMove`, `@_noImplicitCopy`.

A variable or argument is eager-move by satisfying one of two
conditions:

1. Inferred: Its type is eager-move.

2. Annotated: The variable or argument is annotated with an attribute
that specifies it to be: `@_eagerMove`,
`@_noImplicitCopy`.

These source-level rules result in a few sorts of SIL value whose
destroys must not be moved across deinit barriers:

* `begin_borrow [lexical]`
* `move_value [lexical]`
* function arguments
* `alloc_stack [lexical]`

To translate from the source-level representation of lexicality to the
SIL-level representation, for source-level variables (vars, lets, ...)
SILGen generates `begin_borrow [lexical]`, `move_value [lexical]`,
`alloc_stack [lexical]`. For
function arguments, there is no work to do: a
function argument itself can be lexical.

That the first three have constrained lifetimes is encoded in
ValueBase::isLexical, which should be checked before changing the
lifetime of a value.

When a function is inlined into its caller, a lexical borrow scope is
added for each of its @guaranteed arguments, and a lexical move is
added for each of its @owned arguments, (unless the values being passed
are already lexical themselves) ensuring that the lifetimes of the
corresponding source-level values are not shortened in a way that
doesn't respect deinit barriers.

Unlike the other sorts, `alloc_stack [lexical]]` isn't a
SILValue. Instead, it constrains the lifetime of an addressable
variable. Since the constraint is applied to the in-memory
representation, no additional lexical SILValue is required.

### Deinit Barriers

Deinit barriers (see Instruction.isDeinitBarrier(_:)) are instructions
which would be affected by the side effects of deinitializers. To
maintain the order of effects that is visible to the programmer,
destroys of lexical values cannot be reordered with respect to them.
There are three kinds:

1.  synchronization points (locks, memory barriers, syscalls, etc.)
2.  loads of weak or unowned values
3.  accesses of pointers

Examples:

1.  Given an instance of a class which owns a file handle and closes the
    file handle on deinit, writing to the file handle and then
    deallocating the instance would result in changes being written. If
    the destroy of the instance were hoisted above the call to write to
    the file handle, an error would be raised instead.
2.  Given an instance `c` of a class `C` which
    weakly references an instance `d` of a second class
    `D`, if `d` is referenced via a local
    variable `v`, then loading that weak reference from
    `c` within the variable scope should return a non-nil
    reference to `d`. Hoisting the destroy of
    `v` above the weak load from `c`, however,
    would result in the destruction of `d` before that load
    and a nil weak reference to `D`.
3.  Given an instance of a class which owns a buffer and deallocates it
    on deinitialization, accessing the pointer and then deallocating the
    instance is defined behavior. Hoisting the destroy of the instance
    above the access to the memory would result in accessing a freed
    pointer.

## Dead End Blocks

In SIL, one can express that a program is semantically expected to exit
at the end of a block by terminating the block with an
[unreachable](#unreachable). Such a block is called a *program
terminating block* and all blocks that are post-dominated by blocks of
the aforementioned kind are called *dead end blocks*. Intuitively, any
path through a dead end block is known to result in program termination,
so resources that normally would need to be released back to the system
will instead be returned to the system by process tear down.

Since we rely on the system at these points to perform resource cleanup,
we are able to loosen our lifetime requirements by allowing for values
to not have their lifetimes ended along paths that end in program
terminating blocks. Operationally, this implies that:

-   All SIL values must have exactly one lifetime ending use on all
    paths that terminate in a [return](#return) or [throw](#throw). In
    contrast, a SIL value does not need to have a lifetime ending use
    along paths that end in an [unreachable](#unreachable).
-   [end_borrow](#end_borrow) and [destroy_value](#destroy_value) are
    redundant, albeit legal, in blocks where all paths through the block
    end in an [unreachable](#unreachable).

Consider the following legal SIL where we leak `%0` in blocks prefixed
with `bbDeadEndBlock` and consume it in `bb2`:

```
sil @user : $@convention(thin) (@owned Klass) -> @owned Klass {
bb0(%0 : @owned $Klass):
  cond_br ..., bb1, bb2

bb1:
  // This is a dead end block since it is post-dominated by two dead end
  // blocks. It is not a program terminating block though since the program
  // does not end in this block.
  cond_br ..., bbDeadEndBlock1, bbDeadEndBlock2

bbDeadEndBlock1:
  // This is a dead end block and a program terminating block.
  //
  // We are exiting the program here causing the operating system to clean up
  // all resources associated with our process, so there is no need for a
  // destroy_value. That memory will be cleaned up anyways.
  unreachable

bbDeadEndBlock2:
  // This is a dead end block and a program terminating block.
  //
  // Even though we do not need to insert destroy_value along these paths, we
  // can if we want to. It is just necessary and the optimizer can eliminate
  // such a destroy_value if it wishes.
  //
  // NOTE: The author arbitrarily chose just to destroy %0: we could legally
  // destroy either value (or both!).
  destroy_value %0 : $Klass
  unreachable

bb2:
  cond_br ..., bb3, bb4

bb3:
  // This block is live, so we need to ensure that %0 is consumed within the
  // block. In this case, %0 is consumed by returning %0 to our caller.
  return %0 : $Klass

bb4:
  // This block is also live, but since we do not return %0, we must insert a
  // destroy_value to cleanup %0.
  //
  // NOTE: The copy_value/destroy_value here is redundant and can be removed by
  // the optimizer. The author left it in for illustrative purposes.
  %1 = copy_value %0 : $Klass
  destroy_value %0 : $Klass
  return %1 : $Klass
}
```

# Move Only Wrapped Types

## Semantics

Today SIL supports values of move only wrapped type. Given a copyable
type `$T`, the type `$@moveOnly T` is the move only wrapped variant of
the type. This type is always non-trivial even if `$T` itself is trivial
(see discussion below). All move only wrapped types obey the invariant
that they can be copied in Raw SIL but cannot be copied in Canonical SIL
and later SIL stages. This ensures that once we are in Canonical SIL
such values are "move only values" that are guaranteed to never be
copied. This is enforced by:

-   Having SILGen emit copies as it normally does.
-   Use OSSA canonicalization to eliminate copies that aren't needed
    semantically due to consuming uses of the value. This is implemented
    by the guaranteed passes "MoveOnlyObjectChecker" and
    "MoveOnlyAddressChecker". We will emit errors on any of the
    consuming uses that we found in said pass.

Assuming that no errors are emitted, we can then conclude before we
reach canonical SIL that the value was never copied and thus is a "move
only value" even though the actual underlying wrapped type is copyable.
As an example of this, consider the following Swift:

```
func doSomething(@_noImplicitCopy _ x: Klass) -> () { // expected-error {{'x' is borrowed and cannot be consumed}}
  x.doSomething()
  let x2 = x // expected-note {{consuming use}}
  x2.doSomething()
}
```

which codegens to the following SIL:

```
sil hidden [ossa] @doSomething : $@convention(thin) (@guaranteed Klass) -> () {
bb0(%0 : @noImplicitCopy $Klass):
  %1 = copyable_to_moveonlywrapper [guaranteed] %0 : $@moveOnly Klass
  %2 = copy_value %1 : $@moveOnly Klass
  %3 = mark_unresolved_non_copyable_value [no_consume_or_assign] %2 : $@moveOnly Klass
  debug_value %3 : $@moveOnly Klass, let, name "x", argno 1
  %4 = begin_borrow %3 : $@moveOnly Klass
  %5 = function_ref @$s4test5KlassC11doSomethingyyF : $@convention(method) (@guaranteed Klass) -> ()
  %6 = moveonlywrapper_to_copyable [guaranteed] %4 : $@moveOnly Klass
  %7 = apply %5(%6) : $@convention(method) (@guaranteed Klass) -> ()
  end_borrow %4 : $@moveOnly Klass
  %9 = begin_borrow %3 : $@moveOnly Klass
  %10 = copy_value %9 : $@moveOnly Klass
  %11 = moveonlywrapper_to_copyable [owned] %10 : $@moveOnly Klass
  %12 = begin_borrow [lexical] %11 : $Klass
  debug_value %12 : $Klass, let, name "x2"
  end_borrow %9 : $@moveOnly Klass
  %15 = function_ref @$s4test5KlassC11doSomethingyyF : $@convention(method) (@guaranteed Klass) -> ()
  %16 = apply %15(%12) : $@convention(method) (@guaranteed Klass) -> ()
  end_borrow %12 : $Klass
  destroy_value %11 : $Klass
  destroy_value %3 : $@moveOnly Klass
  %20 = tuple ()
  return %20 : $()
} // end sil function '$s4test11doSomethingyyAA5KlassCF'
```

When the move only checker runs upon this SIL, it will see that the
`moveonlywrapped_to_copyable [owned]` is a transitive consuming use of
`%2` (ignoring copies) and that we have a non-consuming use later. So it
will emit an error that we have a guaranteed parameter that is being
consumed. If we remove the assignment to `x2`, then after the move
checker runs successfully we would get the following SIL:

```
sil hidden [ossa] @doSomething : $@convention(thin) (@guaranteed Klass) -> () {
bb0(%0 : @noImplicitCopy $Klass):
  %1 = copyable_to_moveonlywrapper [guaranteed] %0 : $Klass
  debug_value %1 : $@moveOnly Klass, let, name "x", argno 1
  %4 = begin_borrow %1 : $@moveOnly Klass
  %5 = function_ref @$s4test5KlassC11doSomethingyyF : $@convention(method) (@guaranteed Klass) -> ()
  %6 = moveonlywrapper_to_copyable [guaranteed] %4 : $@moveOnly Klass
  %7 = apply %5(%6) : $@convention(method) (@guaranteed Klass) -> ()
  end_borrow %4 : $@moveOnly Klass
  %20 = tuple ()
  return %20 : $()
} // end sil function '$s4test11doSomethingyyAA5KlassCF'
```

yielding SIL without any copies just as we wanted.

At the ABI level, `@moveOnly` does not exist and thus never shows up in
a SILFunctionType's parameters. Instead, we represent it only in the
body of functions and on a function's internal SILArgument
representation. This is since move only wrapped is intended to be a
local power tool for controlling lifetimes rather than a viral type
level annotation that would constrain the type system.

As mentioned above trivial move only wrapped types are actually
non-trivial. This is because in SIL ownership is tied directly to
non-trivialness so unless we did that we could not track ownership
accurately. This loss of triviality is not an issue for most of the
pipeline since we eliminate all move only wrapper types for trivial
types during the guaranteed optimizations after we have run various
ownership checkers but before we have run diagnostics for trivial types
(e.x.: DiagnosticConstantPropagation).

As an example in practice, consider the following Swift:

```
func doSomethingWithInt(@_noImplicitCopy _ x: Int) -> Int {
  x + x
}
```

Today this codegens to the following Swift:

```
sil hidden [ossa] @doSomethingWithInt : $@convention(thin) (Int) -> Int {
bb0(%0 : @noImplicitCopy $Int):
  %1 = copyable_to_moveonlywrapper [owned] %0 : $Int
  %2 = move_value [lexical] %1 : $@moveOnly Int
  %3 = mark_unresolved_non_copyable_value [consumable_and_assignable] %2 : $@moveOnly Int
  %5 = begin_borrow %3 : $@moveOnly Int
  %6 = begin_borrow %3 : $@moveOnly Int
  %7 = function_ref @addIntegers : $@convention(method) (Int, Int Int.Type) -> Int
  %8 = moveonlywrapper_to_copyable [guaranteed] %5 : $@moveOnly Int
  %9 = moveonlywrapper_to_copyable [guaranteed] %6 : $@moveOnly Int
  %10 = apply %7(%8, %9) : $@convention(method) (Int, Int) -> Int
  end_borrow %6 : $@moveOnly Int
  end_borrow %5 : $@moveOnly Int
  destroy_value %3 : $@moveOnly Int
  return %10 : $Int
}
```

once the checker has run, this becomes:

```
sil hidden [ossa] @doSomethingWithInt : $@convention(thin) (Int) -> Int {
bb0(%0 : @noImplicitCopy $Int):
  %1 = copyable_to_moveonlywrapper [owned] %0 : $Int
  %2 = move_value [lexical] %1 : $@moveOnly Int
  %5 = begin_borrow %2 : $@moveOnly Int
  %6 = begin_borrow %2 : $@moveOnly Int
  %7 = function_ref @addIntegers : $@convention(method) (Int, Int Int.Type) -> Int
  %8 = moveonlywrapper_to_copyable [guaranteed] %5 : $@moveOnly Int
  %9 = moveonlywrapper_to_copyable [guaranteed] %6 : $@moveOnly Int
  %10 = apply %7(%8, %9) : $@convention(method) (Int, Int) -> Int
  end_borrow %6 : $@moveOnly Int
  end_borrow %5 : $@moveOnly Int
  return %10 : $Int
}
```

and once we have run the move only wrapped type lowerer, we get the
following SIL:

```
sil hidden [ossa] @doSomethingWithInt : $@convention(thin) (Int) -> Int {
bb0(%0 : @noImplicitCopy $Int):
  %1 = function_ref @addIntegers : $@convention(method) (Int, Int) -> Int
  %2 = apply %1(%0, %0) : $@convention(method) (Int, Int) -> Int
  return %2 : $Int
}
```

exactly what we wanted in the end.

If we are given an owned argument or a let binding, we use a similar
approach. Consider the following Swift:

```
func doSomethingWithKlass(_ x: Klass) -> Klass {
  @_noImplicitCopy let value = x
  let value2 = value
  return value
}
```

A hypothetical SILGen for this code is as follows:

```
sil hidden [ossa] @$s4test20doSomethingWithKlassyAA0E0CADF : $@convention(thin) (@guaranteed Klass) -> @owned Klass {
bb0(%0 : @guaranteed $Klass):
  debug_value %0 : $Klass, let, name "x", argno 1
  %3 = begin_borrow [lexical] %0 : $Klass
  %4 = copy_value %3 : $Klass
  %5 = copyable_to_moveonlywrapper [owned] %4 : $Klass
  %6 = mark_unresolved_non_copyable_value [consumable_and_assignable] %5 : $@moveOnly Klass
  debug_value %6 : $@moveOnly Klass, let, name "value"
  %8 = begin_borrow %6 : $@moveOnly Klass
  %9 = copy_value %8 : $@moveOnly Klass
  %10 = moveonlywrapper_to_copyable [owned] %9 : $@moveOnly Klass
  %11 = begin_borrow [lexical] %10 : $Klass
  debug_value %11 : $Klass, let, name "value2"
  end_borrow %8 : $@moveOnly Klass
  %14 = begin_borrow %6 : $@moveOnly Klass
  %15 = copy_value %14 : $@moveOnly Klass
  end_borrow %14 : $@moveOnly Klass
  end_borrow %11 : $Klass
  destroy_value %10 : $Klass
  destroy_value %6 : $@moveOnly Klass
  end_borrow %3 : $Klass
  %22 = moveonlywrapper_to_copyable [owned] %15 : $@moveOnly Klass
  return %22 : $Klass
} // end sil function '$s4test20doSomethingWithKlassyAA0E0CADF'
```

Notice above how the `moveonlywrapper_to_copyable [owned]` is used to
escape the no implicit copy value. In fact, if one runs the following
SILGen through sil-opt, one will see that we actually have an ownership
violation due to the two uses of "value", one for initializing value2
and the other for the return value.
