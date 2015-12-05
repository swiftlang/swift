orphan

:   

Sequences And Collections in Swift
==================================

Unlike many languages, Swift provides a rich taxonomy of abstractions
for processing series of elements. This document explains why that
taxonomy exists and how it is structured.

Sequences
---------

It all begins with Swift's `for`{.sourceCode}…`in`{.sourceCode} loop:

    for x in s {
      doSomethingWith(x)
    }

Because this construct is generic, `s`{.sourceCode} could be

-   an array
-   a set
-   a linked list
-   a series of UI events
-   a file on disk
-   a stream of incoming network packets
-   an infinite series of random numbers
-   a user-defined data structure
-   etc.

In Swift, all of the above are called **sequences**, an abstraction
represented by the `SequenceType`{.sourceCode} protocol:

    protocol SequenceType { 
      typealias Generator : GeneratorType
      func generate() -> Generator
    }

> **Hiding Generator Type Details**
>
> A sequence's generator is an associated type—rather than something
> like |AnyGenerator|\_\_ that depends only on the element type—for
> performance reasons. Although the alternative design has significant
> usability benefits, it requires one dynamic allocation/deallocation
> pair and *N* dynamic dispatches to traverse a sequence of length *N*.
> That said, our optimizer has improved to the point where it can
> sometimes remove these overheads completely, and we are
> [considering](rdar://19755076) changing the design accordingly.
>
> \_\_ <http://swiftdoc.org/type/AnyGenerator/>

As you can see, sequence does nothing more than deliver a generator. To
understand the need for generators, it's important to distinguish the
two kinds of sequences.

-   **Volatile** sequences like “stream of network packets,” carry their
    own traversal state, and are expected to be “consumed” as they
    are traversed.
-   **Stable** sequences, like arrays, should *not* be mutated by
    `for`{.sourceCode}
    …`in`{.sourceCode}, and thus require *separate traversal state*.

To get an initial traversal state for an arbitrary sequence
`x`{.sourceCode}, Swift calls `x.generate()`{.sourceCode}. The sequence
delivers that state, along with traversal logic, in the form of a
**generator**.

Generators
----------

`for`{.sourceCode}…`in`{.sourceCode} needs three operations from the
generator:

-   get the current element
-   advance to the next element
-   detect whether there are more elements

If we literally translate the above into protocol requirements, we get
something like this:

    protocol NaiveGeneratorType {
      typealias Element
      var current() -> Element      // get the current element
      mutating func advance()       // advance to the next element       
      var isExhausted: Bool         // detect whether there are more elements
    }

Such a protocol, though, places a burden on implementors of volatile
sequences: either the generator must buffer the current element
internally so that `current`{.sourceCode} can repeatedly return the same
value, or it must trap when `current`{.sourceCode} is called twice
without an intervening call to `moveToNext`{.sourceCode}. Both semantics
have a performance cost, and the latter unnecessarily adds the
possibility of incorrect usage.

> **`NSEnumerator`{.sourceCode}**
>
> You might recognize the influence on generators of the
> `NSEnumerator`{.sourceCode} API:
>
>     class NSEnumerator : NSObject {
>       func nextObject() -> AnyObject?
>     }

Therefore, Swift's `GeneratorType`{.sourceCode} merges the three
operations into one, returning `nil`{.sourceCode} when the generator is
exhausted:

    protocol GeneratorType {
      typealias Element
      mutating func next() -> Element?
    }

Combined with `SequenceType`{.sourceCode}, we now have everything we
need to implement a generic `for`{.sourceCode}…`in`{.sourceCode} loop.

> **Adding a Buffer**
>
> The use-cases for singly-buffered generators are rare enough that it
> is not worth complicating `GeneratorType`{.sourceCode}, [^1] but
> support for buffering would fit nicely into the scheme, should it
> prove important:
>
>     public protocol BufferedGeneratorType 
>       : GeneratorType {
>       var latest: Element? {get}
>     }
>
> The library could easily offer a generic wrapper that adapts any
> `GeneratorType`{.sourceCode} to create a \`BufferedGeneratorType\`:
>
>     /// Add buffering to any GeneratorType G
>     struct BufferedGenerator<G: GeneratorType> 
>       : BufferedGeneratorType {
>
>       public init(_ baseGenerator: G) { 
>         self._baseGenerator = baseGenerator
>       }
>       public func next() -> Element? { 
>         latest = _baseGenerator.next() ?? latest
>         return latest 
>       }
>       public private(set) var 
>         latest: G.Element? = nil
>       private var _baseGenerator: G
>     }

### Operating on Sequences Generically

Given an arbitrary `SequenceType`{.sourceCode}, aside from a simple
`for`{.sourceCode}…`in`{.sourceCode} loop, you can do anything that
requires reading elements from beginning to end. For example:

    // Return an array containing the elements of `source`, with
    // `separator` interposed between each consecutive pair.
    func array<S: SequenceType>(
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

    let s = String(array("Swift", withSeparator: "|"))
    print(s)        // "S|w|i|f|t"

Because sequences may be volatile, though, you can—in general—only make
a single traversal. This capability is quite enough for many languages:
the iteration abstractions of Java, C\#, Python, and Ruby all go about
as far as `SequenceType`{.sourceCode}, and no further. In Swift, though,
we want to do much more generically. All of the following depend on
stability that an arbitrary sequence can't provide:

-   Finding a sub-sequence
-   Finding the element that occurs most often
-   Meaningful in-place element mutation (including sorting,
    partitioning, rotations, etc.)

> **Generators Should Be Sequences**
>
> In principle, every generator is a volatile sequence containing the
> elements it has yet to return from `next()`{.sourceCode}. Therefore,
> every generator *could* satisfy the requirements of
> `SequenceType`{.sourceCode} by simply declaring conformance, and
> returning `self`{.sourceCode} from its `generate()`{.sourceCode}
> method. In fact, if it weren't for [current language
> limitations](rdar://17986597), `GeneratorType`{.sourceCode} would
> refine `SequenceType`{.sourceCode}, as follows:
>
> Though we may not currently be able to *require* that every
> `GeneratorType`{.sourceCode} refines `SequenceType`{.sourceCode}, most
> generators in the standard library do conform to
> `SequenceType`{.sourceCode}.

Fortunately, many real sequences *are* stable. To take advantage of that
stability in generic code, we'll need another protocol.

Collections
-----------

A **collection** is a stable sequence with addressable “positions,”
represented by an associated `Index`{.sourceCode} type:

    protocol CollectionType : SequenceType {
      typealias Index : ForwardIndexType             // a position
      subscript(i: Index) -> Generator.Element {get}

      var startIndex: Index {get}
      var endIndex: Index {get}
    }

The way we address positions in a collection is a generalization of how
we interact with arrays: we subscript the collection using its
`Index`{.sourceCode} type:

    let ith = c[i]

An **index**—which must model `ForwardIndexType`{.sourceCode}—is a type
with a linear series of discrete values that can be compared for
equality:

> **Dictionary Keys**
>
> Although dictionaries overload `subscript`{.sourceCode} to also
> operate on keys, a `Dictionary`{.sourceCode}'s `Key`{.sourceCode} type
> is distinct from its `Index`{.sourceCode} type. Subscripting on an
> index is expected to offer direct access, without introducing
> overheads like searching or hashing.

    protocol ForwardIndexType : Equatable {
      typealias Distance : SignedIntegerType
      func successor() -> Self
    }

While one can use `successor()`{.sourceCode} to create an incremented
index value, indices are more commonly advanced using an in-place
increment operator, just as one would when traversing an array:
`++i`{.sourceCode} or `i++`{.sourceCode}. These operators are defined
generically, for all models of `ForwardIndexType`{.sourceCode}, in terms
of the `successor()`{.sourceCode} method.

Every collection has two special indices: a `startIndex`{.sourceCode}
and an `endIndex`{.sourceCode}. In an empty collection,
`startIndex == endIndex`{.sourceCode}. Otherwise,
`startIndex`{.sourceCode} addresses the collection's first element, and
`endIndex`{.sourceCode} is the successor of an index addressing the
collection's last element. A collection's `startIndex`{.sourceCode} and
`endIndex`{.sourceCode} form a half-open range containing its elements:
while a collection's `endIndex`{.sourceCode} is a valid index value for
comparison, it is not a valid index for subscripting the collection:

    if c.startIndex != c.endIndex { } // OK
    c[c.endIndex]                     // Oops! (index out-of-range)

### Mutable Collections

A **mutable collection** is a collection that supports in-place element
mutation. The protocol is a simple refinement of
`CollectionType`{.sourceCode} that adds a subscript setter:

The `CollectionType`{.sourceCode} protocol does not require collection
to support mutation, so it is not possible to tell from the protocol
itself whether the order of elements in an instance of a type that
conforms to `CollectionType`{.sourceCode} has a domain-specific meaning
or not. (Note that since elements in collections have stable indices,
the element order within the collection itself is stable; the order
sometimes does not have a meaning and is not chosen by the code that
uses the collection, but by the implementation details of the collection
itself.)

`MutableCollectionType`{.sourceCode} protocol allows the to replace a
specific element, identified by an index, with another one in the same
position. This capability essentially allows to rearrange the elements
inside the collection in any order, thus types that conform to
`MutableCollectionType`{.sourceCode} can represent collections with a
domain-specific element order (not every instance of a
`MutableCollectionType`{.sourceCode} has an interesting order, though).

### Range Replaceable Collections

The `MutableCollectionType`{.sourceCode} protocol implies only mutation
of content, not of structure (for example, changing the number of
elements). The `RangeReplaceableCollectionType`{.sourceCode} protocol
adds the capability to perform structural mutation, which in its most
general form is expressed as replacing a range of elements, denoted by
two indices, by elements from a collection with a **different** length.

    public protocol RangeReplaceableCollectionType : MutableCollectionType {
      mutating func replaceRange<
        C: CollectionType where C.Generator.Element == Self.Generator.Element
      >(
        subRange: Range<Index>, with newElements: C
      )
    }

### Index Protocols

As a generalization designed to cover diverse data structures,
`CollectionType`{.sourceCode} provides weaker guarantees than arrays do.
In particular, an arbitrary collection does not necessarily offer
efficient random access; that property is determined by the protocol
conformances of its `Index`{.sourceCode} type.

**Forward indices** are the simplest and most general, capturing the
capabilities of indices into a singly-linked list:

1.  advance to the next position
2.  detect the end position

**Bidirectional indices** are a refinement of forward indices that
additionally support reverse traversal:

    protocol BidirectionalIndexType : ForwardIndexType {
      func predecessor() -> Self
    }

Indices into a doubly-linked list would be bidirectional, as are the
indices that address `Character`{.sourceCode}s and
`UnicodeScalar`{.sourceCode}s in a `String`{.sourceCode}. Reversing the
order of a collection's elements is a simple example of a generic
algorithm that depends on bidirectional traversal.

**Random access indices** have two more requirements: the ability to
efficiently measure the number of steps between arbitrary indices
addressing the same collection, and the ability to advance an index by a
(possibly negative) number of steps:

    public protocol RandomAccessIndexType : BidirectionalIndexType {
      func distanceTo(other: Self) -> Distance
      func advancedBy(n: Distance) -> Self
    }

From these methods, the standard library derives several other features
such as `Comparable`{.sourceCode} conformance, index subtraction, and
addition/subtraction of integers to/from indices.

The indices of a
[deque](http://en.wikipedia.org/wiki/Double-ended_queue) can provide
random access, as do the indices into `String.UTF16View`{.sourceCode}
(when Foundation is loaded) and, of course, array indices. Many common
sorting and selection algorithms, among others, depend on these
capabilities.

All direct operations on indices are intended to be lightweight, with
amortized O(1) complexity. In fact, indices into
`Dictionary`{.sourceCode} and `Set`{.sourceCode} *could* be
bidirectional, but are limited to modeling
`ForwardIndexType`{.sourceCode} because the APIs of
`NSDictionary`{.sourceCode} and `NSSet`{.sourceCode}—which can act as
backing stores of `Dictionary`{.sourceCode} and `Set`{.sourceCode}—do
not efficiently support reverse traversal.

Conclusion
----------

Swift's sequence, collection, and index protocols allow us to write
general algorithms that apply to a wide variety of series and data
structures. The system has been both easy to extend, and predictably
performant. Thanks for taking the tour!

------------------------------------------------------------------------

[^1]: This trade-off is not as obvious as it might seem. For example,
    the C\# and C++ analogues for `GeneratorType`{.sourceCode}
    (`IEnumerable`{.sourceCode} and `input iterator`{.sourceCode}) are
    saddled with the obligation to provide buffering.
