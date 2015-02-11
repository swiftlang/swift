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

In Swift, all of the above are called **sequences**, an abstraction
represented by the `SequenceType` protocol.  

Generators
==========

The list of sequences above includes both **volatile** series like
“incoming network packets,” and **stable** ones like “arrays.”  The
*volatile* sequences are expected to be “consumed” by traversal, but
*stable* sequences must not be.  To support stability, traversal state
needs to be stored in an instance separate from the sequence itself.

Therefore, `SequenceType` has a method called `generate`, that creates
the initial state.  The type it returns is called a **generator**:

.. parsed-literal::

  func generate() -> *Generator*

`Generator` is an Associated Type
---------------------------------

Now there is a decision point in the design.  What is the *Generator*
type?  There two basic possibilities:

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
requires one dynamic allocation/deallocation, and another dynamic
dispatch for every element of the sequence.  That said, our optimizer
has improved so that, in common cases, we can remove all of the
overhead of #2, which has some significant usability benefits.  Making
that change at some point is `up for discussion
<rdar://19755076>`_. [#GeneratorOf]_

What's in a Generator?
----------------------

`for…in` needs three things from the generator:

* a way to get the current element
* a way to advance to the next element
* a way to detect that there are no more elements

.. sidebar:: Generators Should Be Sequences

  In principle, every generator is a *volatile* sequence containing the
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
  intended design.

If we literally translate the above into protocol requirements, we get
something like this::

  protocol NaïveGeneratorType {
    typealias Element
    var current() -> Element      // get the current element
    mutating func advance()       // advance to the next element
    var isExhausted: Bool         // detect whether there are more elements
  }

Such a protocol, though, places a burden on implementors of *volatile*
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

Combined with `SequenceType`, we now have everything we need to
implement a generic `for…in` loop.

What About Buffered Access?
---------------------------

For use-cases that actually need "Sequence with a one-element buffer"
semantics, we could one day define another protocol::

  public protocol BufferedGeneratorType : GeneratorType {
    var latest: Element? {get}
  }

a generic wrapper could adapt any `GeneratorType` to create a
`BufferedGeneratorType`::

  /// Add buffering to any GeneratorType G
  struct BufferedGenerator<G: GeneratorType> : BufferedGeneratorType {
    public init(_ baseGenerator: G) { 
      self._baseGenerator = baseGenerator
    }
    public func next() -> Element? { 
      latest = _baseGenerator.next() ?? latest
      return latest 
    }
    public private(set) var latest: G.Element? = nil

    private var _baseGenerator: G
  }

We believe that the use-cases for buffered generators are rare enough
that the simplicity of our `GeneratorType` design is an overall
win. [#input_iterator]_

Operating on Sequences Generically
----------------------------------

Given an arbitrary `SequenceType`, aside from a simple `for…in` loop,
you can do anything that requires going from beginning to end::

  func toArray<S: SequenceType>(
    source: S, 
    withSeparator separator: S.Generator.Element
  ) -> [S.Generator.Element] {
    var result: [S.Generator.Element] = []
    var g = source.generate()
    if let start = g.next() {
      result.append(start)
      while let next = g.next() {
        result.append(separator)
        result.append(next)
      }
    }
    return result
  }

  let s = String(toArray("Swift", withSeparator: "|")) // "S|w|i|f|t"

Because sequences may be *volatile*, though, you can—in general—only
make a single traversal.  This capability is quite enough for many
languages: the iteration abstractions of Java, C#, Python, and Ruby
all go about as far as `SequenceType`, and no further.  You don't look
hard, though, to find tasks like “find a matching sub-sequence,” or
“binary search,” that make sense as generic algorithms, but require a
*stable* data structure.

Fortunately, many real sequences *are* stable; to take advantage of
that stability in generic code, we'll need another protocol.

Collections
===========

Blah blah.

------

.. [#GeneratorOf] The generic type `GeneratorOf<T>` used here is
   actually available in Swift's standard library today.

.. [#input_iterator] This trade-off is not as obvious as it might
   seem.  For example, the C# and C++ analogues for `GeneratorType`
   (`IEnumerable` and `input iterator`) are saddled with the
   obligation to provide buffering.
