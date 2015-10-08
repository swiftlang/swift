:orphan:

.. FunctionEffects:

Function Effects: Analysis, Optimization, and Annotation
========================================================

.. contents::

.. note::

   This is a working document. Once we agree on the approach and
   terminology, this can move into docs/FunctionEffects.rst, and the
   codebase can be cleanup up a bit to reflect the consistent
   terminology.

Introduction
------------

This document formalizes the effects that functions have on program
state for the purpose of facilitating compiler optimization. By
modeling more precise function effects, the optimizer can make more
assumptions leading to more agressive transformation of the program.

Function effects may be deduced by the compiler during program
analyis. However, in certain situations it is helpful to directly
communicate function effects to the compiler via function attributes
or types. These source level annotations may or may not be statically
enforceable.

This document specifies a comprehensive set of primitives and their
semantics. These primitives are the optimizer's interface to function
effects. They should be sufficient to express any analysis or
annotation supported by the compiler that express a function's affect
on program state.

Within the optimizer, SILEffectsAnalysis deduces function effects
through analysis of SIL code. It's result directly maps to effects
primitives.

Optimization of copy-on-Write data structures, such as Array, requires
guaranteed function effects that cannot be deduced through analysis of
SIL code. This necessitates a language for annotating functions. Some
annotations are obviously specific to CoW semantics and cannot be
defined in terms of general effects primitives: make_unique,
preserve_unique, and projects_subobject. However, all other annotations
required for CoW optimization are really guarantees regarding the
program state that is affected by a CoW methods. These annotations
should map precisely to a set of effects primitives.

For the sake of discussion, we use of Swift-level syntax for
specifying effects primitives. It may be debatable whether we actually
want to expose this syntax, but as explained above, some syntax will
need to be exposed to build optimizable CoW types.

.. note::

   [Andy] There is a lot of subtlety involved in specifying or
   summarizing function effects. I want to first put forth an
   underlying model for reasoning about the effects' semantics,
   demonstrate that we can prove soundness in all the cases we care
   about for CoW and any other foreseeable purpose, then go back if
   needed and add sugary syntax and proper defaults for specifying the
   effects. I won't get hung up on the attributes being too fine
   grained or tricky to use.

Effects Primitives
------------------

For the purpose of function effects, we identify program state that is
known reachable via an argument, versus some other unknown/unspecified
state. (Accessing a global variable is always unspecified state.)

[Andy] We've given "global" a variety of meanings. I think we should
avoid that term unless specifically referring to global variables.

There are some function level effects that are not specific to state:

- allocs
- traps

The effects on a particular state are:

- read
- write
- capture
- release

These should all be interpreted as effects that "may"
happen. e.g. maywrite, or mayretain.

[TODO] We currently distinguish between retain and capture, which I
cannot yet justify.

When referring to unspecified state, I will use the syntax
``@effects(no<effectname>)``. When referring to state reachable via an
argument, ``@no<effectname> arg``.

Naturally, we also need a syntax for associating effects with
``self``. That could easily be done by adding a @self_effects
attribute.

In order to optimize bridged types, we need to add a ``nonbridged``
predicate to the effects. The optimizer can then reason about a
value's bridged status within some scope and deduce more optimistic
effects at a call site. For now, we assume the predicate only applies
to unspecified state and that the bridged object is always self. That
way we can denote predicated effects as @nonbridged_effects.

In examples, @effects(argonly) means that there are no effects on
unspecified state.

CoW Optimization Requirements
-----------------------------

[arnold] The following proposal is still worded using the syntax I sent out to
the list.  As Andrew points out this can be recast in terms @effects attributes
and argument attributes.

Copy-on-write type effects proposal
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A copy-on-write (COW) type is implemented in terms of a struct and a set of
storage objects referenced by this struct. The set of storage objects can
further provide storage for subobjects.::

  class ArrayStorage<T> {
    func getElement(index: Int) -> T {} // Return a 'subobject'.
  } // Storage object.

  struct Array<T> {
    var storage: ArrayStorage
  }

The following effects attributes can be used to describe properties of methods
of such a datastructure to facilitate optimization.

An instance of a struct is in a uniqued state if changes to the set of storage
objects can only be observed by method calls on references to the instance of
the struct (versus by method calls on other instances). Typically, one would
implement this behavior by checking whether the references to the storage
objects are uniquely referenced and copying the storage objects on modification
if they are not.  In the following we refer to the memory holding the instance
of the struct and the set of storage objects as the self state. Global state
below refers to the state of the rest of the program not including the self
state.

makeunique

Using ``@effects(makeunique)`` on a method implies that the method makes the
self object referenced by the self argument unique without externally visible
side-effects and without depending on global state. It is readnone with respect
to global state.

Example::

  struct Array<T> {
    var storage: ArrayStorage

    @effects(makeunique)
    func makeUnique() {
      if (isUniquelyReferenced(&storage))
        return
      storage = storage.copy()
    }

preserveunique

Using ``@effects(preserveunique)`` on a method implies that the method preserves
the uniqueness state of the self argument.

Example:::

  struct Array<T> {
    var storage: ArrayStorage

    @effects(preserveunique, captureonly)
    appendAssumingUnique(captureonly e: T) {
      storage.append(e)
    }

noaliasingprojectsubobject

Using ``@effects(noaliasingprojectsubobject)`` on a method implies the method
returns a 'subobject' that is stored by the set of storage objects. It is
guaranteed that the 'subobject' returned is kept alive as long the current value
of the 'self' object is alive. Capturing the returned 'subobject' does not
capture the 'self' object.

Example:::

  struct Array<T> {
    var storage: ArrayStorage

    @effects(preserveunique, noaliasingprojectsubobject,
             readnone_global_nonbridged, readonly_self)
    getElement(index: Int) -> T {
      // Returns a 'subobject'.
      return storage.elementAt(index)
    }

noaliasingprojectsubobjectaddr

Using ``@effects(noaliasingprojectsubobjectaddr)`` on a method implies the
method returns the address of a 'subobject' that is stored by the set of storage
objects. It is guaranteed that the 'subobject' at the returned address is kept
alive as long the current value of the 'self' object is alive.  A store to the
address of the returned 'subobject' is guaranteed not to change the uniqueness
state of the 'self' object. Capturing the value obtained by loading the returned
'subobject' address does not capture the 'self' object.

Example:::

  struct Array<T> {
    var storage: ArrayStorage

    @effects(preserveunique, noaliasingprojectsubobjectaddr, readnone_global,
             readonly_self)
    getElementAddress(index: Int) -> UnsafeMutablePointer<T> {
      return storage.elementAddressAt(index)
    }

argonlyglobaleffects

The method does not have any global side effect and does not read global state
other than the effects described by attributes on the method's arguments listed
below.

Argument attributes:

  ``@capture``: the method captures this argument.

  ``@release``: the method will release an element of the type of this argument

Example:::

  struct Array<T> {
    var storage: ArrayStorage

    @effects(preserveunique, argonlyglobaleffects)
    func appendAssumingUnique(@capture e: T) {
      storage.append(e)
    }

    @effects(preserveunique, argonlyglobaleffects)
    func setElement(@capture @release e: T, index: Int) {
      storage.set(e, index)
    }

readnone_global

The method is guaranteed not to change or depend on global state, it may read
or modify the self state.

Example:::

  struct Array<T> {
    var storage: ArrayStorage

    @effects(preserveunique, noaliasingprojectsubobjectaddr, readnone_global,
             readonly_self)
    func getElementAddress(index: Int) -> UnsafeMutablePointer<T> {
      storage.elementAddressAt(index)
    }

readnone_global_nonbridged

The method is guaranteed not to change or depend on global state, it may read
or modify the self state if the self object can be shown (by the optimizer) to
not be in a bridged state.

Example:::

  struct Array<T> {
    var storage: ArrayStorage

    @effects(preserveunique, noaliasingprojectsubobject,
             readnone_global_nonbridged, readonly_self)
    func getElement(index: Int) -> T {
      if storage.isObjC {
        return storage.getElementObjC(index)
      } else {
        return storage.elementAddressAt(index).value
      }


readonly_self

The method only reads from self state. It may read or write global state.

Example:::

  struct Array<T> {
    var storage: ArrayStorage

    @effects(preserveunique, readnone_global, readonly_self)
    func count() -> Int {
      storage.count
    }

Motivation
~~~~~~~~~~

Why do we need ``makeunique``, ``preserveunique``, and
``noaliasingprojectsubobjectaddr``?

The optimizer wants to hoist functions that make a COW type instance unique out
of loops. In order to do that it has to prove that uniqueness is preserved by
all operations in the loop.

Marking methods as ``makeunique``/``preserveunique`` allows the optimizer to
reason about the behavior of the method calls.

Example:::

  struct Array<T> {
    var storage: ArrayStorage<T>

    @effects(makeunique)
    func makeUnique() {
      if (isUniquelyReferenced(&storage))
       return;
      storage = storage.copy()
    }

    @effects(preserveunique, noaliasingprojectsubobjectaddr, readnone_global)
    func getElementAddr(index: Int) -> UnsafeMutablePointer<T> {
      return storage.elementAddrAt(index)
    }

    subscript(index: Int) -> UnsafeMutablePointer<T> {
      mutableAddressor {
        makeUnique()
        return getElementAddr(index)
      }
    }
  }

When the optimizer optimizes a loop:::

  func memset(inout A: [Int], value: Int) {
    for i in 0 .. A.size {
      A[i] = value
      f()
    }
  }

It will see the following calls. @effect methods are not inlined.::

  func memset(inout A: [Int], value: Int) {
    for i in 0 .. A.size {
      makeUnique(&A)
      addr = getElementAddr(i, &A)
      addr.memory = value
      f()
    }
  }

In order to hoist the 'makeUnique' call, the optimizer needs to be able to
reason that neither 'getElementAddr', nor the store to the address returned can
change the uniqueness state of 'A'. Furthermore, it knows because 'A' is marked
inout that in a program without inout violations f cannot hold a reference to
the object named by 'A' and therefore cannot modify it.

Why do we need ``argonlyglobaleffects``, ``readnone_global``?

We want to be able to hoist ``makeunique`` calls when the array is not identfied
by a unique name::

  class AClass {
    var array: [Int]
  }

  func copy(a : AClass, b : AClass) {
    for i in min(a.size, b.size) {
       a.array.append(b.array[i])
    }
  }

In such a case we would like to reason that:::

  = b.array[i]

cannot changed the uniqueness of the instance of array 'a' assuming 'a' !=== 'b'.
We can do so because 'getElement' is marked readnone_global.

Further we would like to reason that:::

  a.array.append

cannot change the uniqueness state of the instance of array 'b'. We can conclude
so because the element passed to appendAssumingUnique is of type Int.  A trivial
type like Int is not retained when it is captured and therefore cannot change
the uniqueness state of any array.::

  for i in 0 .. b.size {
    tmp = getElement(b.array, i)
    makeUnique(&a.array)
    // @effects(argonlyglobaleffects)
    appendAssumingUnique(&a.array, @capture tmp)
  }

What if the element type would a non-trivial type? Let's assume it is a
non-trivial struct type. In this case we know that 'getElement' the 'subobject'
cannot be used to capture b.array. However the return subobject could be a
semantic copy of a.array. It is still valid to hoist here because before we
append we make a.array unique and preserve the semantic copy.

We can construct a very similar example where we cannot hoist makeUnique. If we
replace 'getElement' with a 'setElement'. 'setElement' will capture its argument
and further releases an element of type T ::

 @effects(argonlyglobaleffects)
 func setElement(@capture @release e: T, index: Int) {
   storage->setElement(e, index)
 }

Depending on 'T''s type a destructor can be invoked by the release on 'e'. The
destructor can have arbitrary side-effects. Therefore, it is not valid to hoist
the makeUnique in the code without proving that 'T's destructor cannot change
the uniqueness state. This is trivial for trivial types but requires a more
sophisticated analysis for class types (and in general cannot be disproved). In
following example we can only hoist makeUnique if we can prove that  elt's, and
elt2's destructor can't change the uniqueness state of the arrays.::

 for i in 0 ..< min(a.size, b.size) {
   makeUnique(&b.array)
   setElement(&b.array, i, elt)
   makeUnique(&a.array)
   setElement(&a.array, i, elt2)
 }

In the the following loop it is not safe to hoist the makeUnique(&a)
call even for trivial types. 'appendAssumingUnique' captures its argument 'a'
which forces a copy on 'a' on every iteration of the loop.::

  for i in 0 .. a.size {
    makeUnique(&a)
    setElement(&a, 0, 1)
    makeUnique(&b)
    appendAssumingUnique(&b, a)
  }

To support this reasoning we need to know when a function captures its
arguments and when a function might release an object and of which type.

Why do we need readonly_self?

This will allow us to remove redundant calls to readonly methods on
COW type instances assuming we can prove that the array instance is not
changed in between them.::

  func f(a: [Int]) {
   //@effects(readnone_global, readonly_self)
   count(a)
   //@effects(readnone_global, readonly_self)
   count(a)
  }

Mapping to effects primitives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For each term in Arnold's proposal, here is a typical mapping to effects
primitives:

``argonlyglobaleffects``:

  @effects(argonly); @nowrite @norelease arg

``readnone_global[_nonbridged]``: 

  @nonbridged_effects(argonly); @nowrite @nocapture self

``readonly_self``:

  @effects(argonly); @nowrite @nocapture self

[arnold] The @effects(argonlyglobaleffects) '@release arg' from the proposal
cannot be expressed using the primitives outline so far because we want it to
say that we may release an object of the type of the argument.

A function argument annotated with @capture only versus a function argument
annotated with @capture and @release expresses the distinction between:::

   copy_addr [init] some_dest, arg // retains arg
   copy_addr some_dest,arg  // retains arg, releases some_dest

We could express this using positive argument effects: ``@captureinit`` vs ``@capture``.

Using a negative forumlation is a lot less intuitive though and not safe in case
of omission.  ``@noinitcapture arg`` would imply may-capture and may-release
while ``(empty) arg`` implies may-capture but not may-release.

Maybe the answer is to indeed add a ``@no_release_type_of_argument`` attribute?


Examples of Optimization Using Effects Primitives
-------------------------------------------------

CoW optimization: [Let's copy over examples from Arnold's proposal]
[See Copy-on-write proposal above]

String initialization: [TBD]

User-Specified Effects, Syntax and Defaults
-------------------------------------------

Mostly TBD.

The optimizer can only take advantage of user-specified effects before
they have been inlined. Consequently, the optimizer initialy preserves
calls to annotated @effects() functions. After optimizing for effects
these functions can be inlined, dropping the effects information.

Without special syntax, specifying a pure function would require::

  @effects(argonly)
  func foo(@noread @nowrite arg)

A shorthand, such as @effects(none) could easily be
introduced. Typically, this shouldn't be needed because the purity of
a function can probably be deduced from its argument types given that
it has no effect on unspecified state. i.e. If the function does not
affect unspecific state, and operates on "pure value types" (see
below), the function is pure.

Specifying Effects for Generic Functions
----------------------------------------

Specifying literal function effects is not possible for functions with
generic arguments::

  struct MyContainer<T> {
    var t: T
    func setElt(elt: T) { t = elt }
  }

With no knowledge of T.deinit() we must assume worst case. SIL effects
analysis following specialization can easily handle such a trivial
example. But there are two situations to be concerned about:

1. Complicated CoW implementations defeat effects analysis. That is
   the whole point of Arnold's proposal for user-specified CoW
   effects.

2. Eventually we will want to publish effects on generic functions
   across resilience boundaries.

Solving this requires a system for polymorphic effects. Language
support for polymorphic effects might look something like this::

  @effects(T.release)
  func foo<T>(t: T) { ... }

This would mean that foo's unspecified effects are bounded by the
unspecified effects of T's deinitializer. The reality of designing
polymorphic effects will be much more complicated.

A different approach would be to statically constrain effects on
generic types, protocol conformance, and closures. This wouldn't solve
the general problem, but could be a very useful tool for static
enforcement.

Purity
------

Motivation for Pure Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An important feature of Swift structs is that they can be defined such
that they have value semantics. The optimizer should then be able to
reason about these types with knowledge of those value semantics. This
in turn allows the optimizer to reason about function purity, which is
a powerful property. In particular, calls to pure functions can be
hoisted out of loops and combined with other calls taking the same
arguments. Pure functions also have no detrimental effect on
optimizing the surrounding code.

For example::

  func bar<T>(t: T) {...}
   
  func foo<T>(t: T, N: Int) {
    for _ in 1...N {
      bar(t)
      bar(t)
    }
  }

With some knowledge of bar() and T can become::

  func foo<T>(t: T, N: Int) {
    bar(t)
  }

If our own implementation of value types, like Array, Set, and String
where annotated as know "pure values" and if their common operations
are known to comply with some low-level effects, then the optimizer
could infer more general purity of operations on those types. The
optimizer could then also reason about purity of operations on user
defined types composed from Arrays, Sets, and Strings.

"Pure" Value Types
~~~~~~~~~~~~~~~~~~

Conceptually, a pure value does not share state with another
value. Any trivial struct is automatically pure. Other structs can be
declared pure by the author. It then becomes the author's
resonsibility to guarantee value semantics. For instance, any stored
reference into the heap must either be to immutable data or protected
by CoW.

Since a pure value type can in practice share implementation state, we
need an enforcable definition of such types. More formally:

- Copying or destroying a pure value cannot affect other program
  state.

- Reading memory referenced from a pure value does not depend on other
  program state. Writing memory referenced from a pure value cannot
  affect other program state.

The purity of functions that operate on these values, including their
own methods, must be deduced independently.

From the optimizer perspective, there are two aspects of type purity
that fall out of the definition:

(1) Side Effects of Copies

    Incrementing a reference count is not considered a side effect at
    the level of value semantics.  Destroying a pure value only
    destroys objects that are part of the value's storage. This could
    be enforced by prohibiting arbitrary code inside the storage deinitializer.

(2) Aliasing

    Mutation of the pure value cannot affect program state apart from that value,
    AND writing program state outside the value cannot affect the pure value.

[Note] Reference counts are exposed through the isUniquelyReferenced
API. Since copying a pure value can increase the reference of the
storage, strictly speaking, a pure function can have user-visible side
effects. We side step this issue by placing the burden on the user of
the isUniquelyReferenced API. The compiler only guarantees that the
API returns a non-unique reference count if there does happen to be an
aliasing reference after optimization, which the user cannot
control. The user must ensure that the program behaves identically in
either case apart from its performance characteristics.

Recognizing Value Types
~~~~~~~~~~~~~~~~~~~~~~~

A major difficulty in recognizing value types arises when those types
are implemented in terms of unsafe code with arbitrary side
effects. This is the crux of the difficulty in defining the CoW
effects. Consequently, communicating purity to the compiler will
require some function annotations and/or type constraints.

Erik suggested that a CoW-implemented value type have its storage
annotated. The compiler can then defer inlining methods that expose
the storage (this is a generalization of the current Array
design). The compiler would need to treat calls to those
implementation methods as an optimization boundary until it
effectively lowers value types. After value type lowering, the
compiler would no longer be able to consider those CoW types as value
types anywhere in the code. I think this would simplify optimization
of nonmutating operations on CoW types; however, most of Arnold's work
has been to support optimization across mutating CoW operations, which
will still require highly complex logic.

As discussed above, CoW types will often be generic, making the
effects of an operation on the CoW type dependent on the effects of
destroying an object of the element type.

TODO: Need more clarity and examples

Inferring Function Purity
~~~~~~~~~~~~~~~~~~~~~~~~~

The optimizer can infer function purity by knowing that (1) the
function does not access unspecified state, (2) all arguments are pure
values, and (3) no calls are made into nonpure code.

(1) The effects system described above already tells the optimizer via
    analysis or annotation that the function does not access
    unspecified state.

(2) Copying or destroying a pure value by definition has no impact on
    other program state. The optimizer may either deduce this from the
    type definition, or it may rely on a type constraint.

(3) Naturally, any calls within the function body must be transitively
    pure. There is no need to check a calls to the storage
    deinitializer, which should already be guaranteed pure by virtue
    of (2).

Mutability of a pure value should not affect the purity of functions
that operate on the value. An inout argument is semantically nothing
more than a copy of the value.

[Note] Pure functions do not depend on or imply anything about the
reference counting effects: capture and release. Optimizations that
depend on reference count stability, like uniqueness hoisting, cannot
treat pure functions as side-effect free.

[Andy] It may be possible to make some assumptions about immutability
of ``let`` variables, which could lead to similar optimization.

TODO: Need more clarity and examples

Closures
--------

Mostly TBD.

The optimizer does not currently have a way of statically determining
or enforcing effects of a function that takes a closure. We could
introduce attributes that statically enforce constraints. For example,
and @pure closure would only be permitted to close over pure values.
[Andy] That is a fairly strict requirement, but not one that I know
how to overcome.

Thread Safety
-------------

The Swift concurrency proposal refers to a ``Copyable`` type. A type
must be Copyable in order to pass it across threads via a
``gateway``. The definition of a Copyable type is equivalent to a
"pure value". However, it was also proposed that the programmer be
able to annotate arbitrary data types as Copyable even if they contain
shared state as long as it is protected via a mutex. However, such
data types cannot be considered pure by the optimizer. I instead
propose that a separate constraint, Synchronized, be attributed to
shareable types that are not pure. An object could be passed through a
gateway either if it is a PureValue or is Synchronized.

Annotations for thread safety run into the same problems with generics
and closures.

API and Resilience
------------------

Any type constraints, function effects, or closure attributes that we
introduce on public functions become part of the API.

Naturally, there are resilience implications to user-specified
effects. Moving to a weaker set of declared effects is not resilient.

Generally, a default-safe policy provides a much better user model
from some effects. For example, we could decide that functions cannot
affect unspecified state by default. If the user accesses globals,
they then need to annotate their function. However, default safety
dictates that any neccessary annotations should be introduced before
declaring API stability.
