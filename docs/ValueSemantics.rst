.. _ValueSemantics:

==========================
 Value Semantics in Swift
==========================

**Abstract**: Swift is the first language to take Generic Programming
 seriously that also has both value and reference types.  The
 (sometimes subtle) differences between the behaviors of value and
 reference types create unique challenges for generic programs that we
 have not yet addressed.  This paper surveys related problems
 and explores some possible solutions.


Definitions
===========

I propose the following definitions of "value semantics" and
"reference semantics."

Value Semantics
---------------

For a type with value semantics, variable initialization, assignment,
and argument-passing (hereafter, “the big three operations”) each
create an *independently modifiable copy* of the source value that is
*interchangeable with the source*. [#interchange]_

If `T` has value semantics, the `f`\ s below are all equivalent::

  func f1() -> T {
     var x : T
     return x
  }

  func f2() -> T {
     var x : T
     var y = x
     return y  // a copy of x is equivalent to x
  }

  func f2a() -> T {
     var x : T
     var y : T
     y = x
     return y  // a copy of x is equivalent to x
  }

  func f3() -> T {
     var x : T
     var y = x
     y.mutate() // a copy of x is modifiable
     return x   // without affecting x
  }

  func f3a() -> T {
     var x : T
     var y : T
     y = x;
     y.mutate() // a copy of x is modifiable
     return x   // without affecting x
  }

  func g(x : T) { x.mutate() }

  func f4() -> T {
     var x : T
     g(x)         // when x is passed by-value the copy
     return x     // is modifiable without affecting x
  }


Reference Semantics
-------------------

Values of a type with reference semantics are only accessible
indirectly, via a reference.  Although swift tries to hide this fact
for simplicity, for the purpose of this discussion it is important to
note that there are always *two* values in play: the value of the
reference itself and that of the object being referred to (a.k.a. the
target).

The programmer thinks of the target's value as primary, but it is
never used as a variable initializer, assigned, or passed as a
function argument.  Conversely, the reference itself has full value
semantics, but the user never sees or names its type.  The programmer
expects that copies of a reference share a target object, so
modifications made via one copy are visible as side-effects through
the others.

If `T` has reference semantics, the `f`\ s below are all
equivalent::

  func f1(T x) {
     x.mutate()
     return x
  }

  func f2(T x) -> T {
     var y = x
     y.mutate()  // mutation through a copy of x
     return x    // is visible through x
  }

  func f2a(T x) -> T {
     var y : T
     y = x
     y.mutate()  // mutation through a copy of x
     return x    // is visible through x
  }

  func g(x : T) { x.mutate() }

  func f3(T x) -> T {
     g(x)        // when x is passed to a function, mutation
     return x    // through the parameter is visible through x
  }

The Role of Mutation
--------------------

It's worth noting that in the absence of mutation, value semantics and
reference semantics are indistinguishable.  You can easily prove that
to yourself by striking the calls to `mutate()` in each of the
previous examples, and seeing that the equivalences hold for any type.
In fact, the fundamental difference between reference and value
semantics is that **value semantics never creates multiple paths to
the same mutable state**. [#cow]_

.. Admontion:: `struct` vs `class`

   Although `struct`\ s were designed to support value semantics and
   `class`\ es were designed to support reference semantics, it would
   be wrong to assume that they are always used that way.  As noted
   earlier, in the absence of mutation, value semantics and reference
   semantics are indistinguishable.  Therefore, any immutable `class`
   trivially has value semantics (*and* reference semantics).

   Second, it's easy to implement a `struct` with reference semantics:
   simply keep the primary value in a `class` and refer to it through an
   instance variable.  So, one cannot assume that a `struct` type has
   value semantics.  `Slice` is an example of a reference-semantics
   `struct` from the standard library.


The Problem With Generics
=========================

The classic Liskov principle says the semantics of operations on
`Duck`\ 's subtypes need to be consistent with those on `Duck` itself,
so that functions operating on `Duck`\ s still “work” when passed a
`Mallard`.  More generally, for a function to make meaningful
guarantees, the semantics of its sub-operations need to be consistent
regardless of the actual argument types passed.

The type of an argument passed by-value to an ordinary function is
fully constrained, so the “big three” have knowable semantics.  The
type of an ordinary argument passed by-reference is constrained by
subtype polymorphism, where a (usually implicit) contract between
base- and sub-types can dictate consistency.

However, the situation is different for functions with arguments of
protocol or parameterized type.  In the absence of specific
constraints to the contrary, the semantics of the big three can vary.

For example, there's an algorithm called ``cycle_length`` that
measures the length of a cycle of states (e.g. the states of a
pseudo-random number generator).  It needs to make one copy and do
in-place mutation of the state, rather than wholesale value
replacement via assignment, which might be expensive.

Here’s a version of cycle_length that works when state is a mutable
value type::

 func cycle_length<State>(
   s : State, mutate : ([byref]State)->()) -> Int
   requires State : EqualityComparable
 {
     State x = s    // one copy                // 1
     mutate(x)      // in-place mutation
     Int n = 1
     while x != s {                            // 2
          mutate(x) // in-place mutation
          ++n
     }
     return n
 }

The reason the above breaks when the state is in a class instance is
that the intended copy in line 1 instead creates a new reference to
the same state, and the comparison in line 2 (regardless of whether we
decide ``!=`` does “identity” or “value” comparison) always succeeds.

You can write a different implementation that only works on clonable
classes (assuming value comparison semantics for ``!=``):

.. parsed-literal::

 func cycle_length<State>(
   s : State, mutate : ([byref]State)->()) -> Int
   requires State : EqualityComparable, **Clonable**
 {
     State x = s\ **.clone()** // one copy
     *etc.*

You could also redefine the interface so that it works on both values and
clonable classes:

.. parsed-literal::

 func cycle_length<State>(
   s : State, **next : (State)->State**) -> Int
   requires State : EqualityComparable
 {
     State **x = next(s)**
     Int n = 1
     while x != s {
          **x = next(x)**
          ++n
     }
     return n
 }

However, this implementation makes O(N) separate copies of the state.
I don't believe there's a reasonable way write this so it works on
clonable classes, non-classes, and avoids the O(N)
copies. [#extension]_

The Role of Moves
=================

Further complicating matters is the fact that the big three operations
can be—and often are—combined in ways that mask the value/reference
distinction.  Take, for example, `swap`, which uses variable
initialization and assignment to exchange two values::

  var tmp = lhs
  lhs = rhs
  rhs = tmp

This implementation “just works” for types with either value or
reference semantics.  When the type referred to by `lhs` and `rhs` have
reference semantics, primary values are exchanged along with the
reference values.  `swap` is reference-agnostic because shared mutable
state doesn't persist.

In fact, any algorithm whose copies can be replaced
with destructive moves (where the


Furthermore, any operation whose
mutations can be implemented
entirely in terms of

This equivalence is due to the fact that `swap` is
built on *moves*, rather than copies.  

You can use copies to implement `swap`, but the same algorithm worksWe could imagine a hypothetical
syntax for moving in swift, where (unlike assignment) the value of the
right-hand-side of the `<-` is not necessarily preserved::

  var tmp <- lhs
  lhs <- rhs
  rhs <- tmp

Such operations are safe to use in generic code without regard to the
differences between value- and reference- semantics.  If this syntax
were extended to handle function arguments, it would cover the "big
three" operations::

  f(<-x)

How to Build an Interesting Type with Value Semantics
=====================================================

Suppose we want to build a variable-sized data structure `X` with
(mutable) value semantics?  How do we do it?  

If we make `X` a `class`, we automatically get reference semantics, so
its value must be copied before each mutation, which is tedious and
error-prone.  Its public mutating interface must be in terms of free
functions (not methods), so that the original reference value can be
passed `[byref]` and overwritten.  Since there's no user access to the
reference count, we can't determine that we hold the only reference to
the value, so we can't optimize copy-on-write, even in single-threaded
programs.  In multi-threaded programs, where each mutation implies
synchronization on the reference count, the costs are even higher.

If we make the type a `struct`, you have only two ways to create
variable-sized data:

1. Hold a type with reference semantics as an instance variable.
   Unfortunately, this is really nothing new; we must still implement
   copy-on-write.  We can, however, use methods for mutation in lieu
   of free functions.

2. Use discriminated unions (`oneof`).  Interestingly, a datatype
   built with `oneof` automatically has value semantics.  However,
   there vocabulary of efficient data structures that can be built
   this way is extremely limited.  For example, while a singly-linked
   list is trivial to implement, an efficient doubly-linked list is
   effectively impossible.

What Sucks About Reference Semantics
====================================

* Thread safety is difficult and expensive
* Correctness is difficult
  * Defensive copying
* 

What Sucks about Value Semantics
================================

* Copies are easy to make by mistake and may be expensive.
* Some programmers may not be expecting value semantics

Remedies
========

Because non-generic functions normally don't have this problem and
there is no tradition among generic programmers of explicitly handling
reference semantics differently from value semantics, we can't expect
existing habits to make this issue go away.

================================
 IGNORE EVERYTHING FROM HERE ON
================================

Options
=======

var x: T
val x :T
ref x :T

The Problem
===========

The problem boils down to this: value and reference types share syntax
for fundamental operations

Goals
=====

- Make value types easy to build and use
- Make it possible to achieve highest efficiency
- Make it possible to write generic programs
- Make it easy to work with handle types

- What is the relationship between byref and auto-closure?  Wouldn't
  it be nice to do byref with auto-closure?  I think that may imply
  admitting lvalues if increment(x[1]) is going to work.


Problems
========

::

  protocol Number {
      func [infix] *=(x [byref]: Number, y: Number)->Number
  }

  func <T: Number> square(T n) {
      n *= n
      return n
  }


- Ease of building value types
- Acknowledge the weight of copy c'tor, move c'tor, yada yada
- Mutating values inside functions

Reference Protocol
==================

It would be nice to be able to define "Reference" as a protocol, where
you're always (in principle) going through getters and setters.

Write Combining
===============

Doug wants the right to avoid calling getters and setters for each
mutation to an object, so instead of encoding::

  func grow(r : [byref] Rect) -> Void
  {
      --r.left
      --r.top
      ++r.bottom
      ++r.right
  }

as::

  void grow(RectProperty r)
  {
      {
          Rect temp = r.get();
          --temp.left;
          r.set(temp);
      }

      {
          Rect temp = r.get();
          --temp.top;
          r.set(temp);
      }

      // etc...
  }

we can instead do:

  void grow(RectProperty r)
  {
      Rect temp = r.get();
      --temp.left
      --temp.top
      ++temp.bottom
      ++temp.right
      r.set(temp)
  }

What are the rules about what you're allowed to count on?  I think the
compiler has to prove there are no observable differences in order to
make this optimization.  We must require that the programmer obey the
law that

   r.set(x).get() == x

Kinds of Mutation
=================

`someclass.mutating_member()` modifies the referent shared by all someclass'es
`someclass += 3` may be COW, only modifying the thing someclass is referring to.

Crazy Ideas
===========

Notes
=====

----

.. [#interchange] Technically, copies of objects with value semantics
                  are interchangeable until they're mutated.
                  Thereafter, the copies are interchangeable except
                  insofar as it matters what value type they are
                  *aggregated into*.

.. [#cow] Note that this definition *does* allow for value semantics
              using copy-on-write

.. [#extension] I can think of a language extension that would allow
                this, but it requires creating a protocol for generic
                copying, adding compiler magic to get both classes and
                structs to conform to it, and telling generic
                algorithm and container authors to use that protocol
                instead of ``=``, which IMO is really ugly and
                probably not worth the cost.
