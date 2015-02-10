:orphan:

.. @raise litre.TestsAreMissing
.. default-role:: code

=========================================
 The Design of Sequences And Collections
=========================================

This document explains Swift's generic abstractions for linearly
traversible series of elements.  

Sequences
=========

Swift has a generic `for…in` loop::

  for…in series {
    doSomethingWith(x)
  }

Because it is generic, we can apply it to

* Arrays
* Sets
* Linked lists
* UI events
* Files
* Incoming network packets
* Infinite series of random numbers
* User-defined data structures we've never seen

In swift, all of these are conceptually called **sequences**, an
abstraction represented by the `SequenceType` protocol.  Before we can
explain the details of `SequenceType`, however, we have to look at a
few design constraints.

Generators
==========

The list of sequences above includes both volatile series like
“incoming network packets,” and stable ones like arrays.  The volatile
sequences are expected to be “consumed” by traversal, but stable
sequences must not be.  Therefore, `SequenceType` has a method to
produce a *separate instance* of traversal state.  That method is
called `generate` and the type it returns is called a **generator**:

.. parsed-literal::

  func generate() -> *Generator*

The Generator is an Associated Type
-----------------------------------

Now there is a decision point in the design.  What is the *Generator*
type?  There two main choices are

1. an associated type of the `SequenceType` protocol:

  .. parsed-literal::

    protocol Sequence {
      **typealias Generator**
      func generate() -> **Generator**
    }

2. a type that depends only on the Sequence's element type:

  .. parsed-literal::

    protocol Sequence {
      **typealias Element**
      func generate() -> **GeneratorOf<Element>**
    }
     
Swift chose #1 mostly for performance reasons: #2, in principle,
requires one dynamic allocation/deallocation, and a dynamic dispatch
for every element of the sequence.  As our optimizer has improved, it
turns out that in common cases we can remove all of the overhead of
#2, which has some significant usability benefits.  Making that change
at some point is `up for discussion <rdar://19755076>`_.

What's in a Generator?
----------------------

`for…in` needs three things from the generator:

* a way to get the current element
* a way to advance to the next element
* a way to detect that there are no more elements

.. sidebar:: Generators Should Be Sequences

  In principle, every generator is a volatile sequence containing the
  elements it has yet to return from `next()`.  Therefore, every
  generator could satisfy the requirements of `SequenceType` by simply
  returning `self` from its `generate()` method.  In fact, if it weren't
  for `current language limitations <rdar://17986597>`_, `GeneratorType`
  would refine `SequenceType`, as follows:

  .. parsed-literal::

       protocol GeneratorType **: SequenceType** {
         typealias Element
         mutating func next() -> Element?
       }

  Keeping that ideal in mind may be helpful in underestanding the
  intended design here.

If we literally translate the above into protocol requirements, we get
something like this::

  protocol NaïveGeneratorType {
    typealias Element
    var current() -> Element      // get the current element
    mutating func advance()       // advance to the next element
    var isExhausted: Bool         // detect whether there are more elements
  }

Such a protocol, though, places a burden on implementors of volatile
sequences: either the generator must buffer the current element
internally so that `current` can repeatedly return the same value, or
it must trap when `current` is called twice without an intervening
call to `moveToNext`.  Both semantics have a performance cost, and
the latter unnecessarily adds the possibility of incorrect usage.

Therefore, Swift's `GeneratorType` combines the three methods into one
that returns `nil` when the generator is exhausted::

  protocol GeneratorType {
    typealias Element
    mutating func next() -> Element?
  }

