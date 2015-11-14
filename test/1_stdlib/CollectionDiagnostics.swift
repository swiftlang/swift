// RUN: %target-parse-verify-swift

import StdlibUnittest

//
// Check that Collection.SubSequence is constrained to Collection.
//

// expected-error@+2 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'Sequence'}}
// expected-error@+1 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'Collection'}}
struct CollectionWithBadSubSequence : Collection {
  var startIndex: MinimalForwardIndex {
    fatalError("unreachable")
  }

  var endIndex: MinimalForwardIndex {
    fatalError("unreachable")
  }

  subscript(i: MinimalForwardIndex) -> OpaqueValue<Int> {
    fatalError("unreachable")
  }

  // expected-note@+2 {{possibly intended match 'SubSequence' (aka 'OpaqueValue<Int8>') does not conform to 'Indexable'}}
  // expected-note@+1 {{possibly intended match}}
  typealias SubSequence = OpaqueValue<Int8>
}

func useCollectionTypeSubSequenceIndex<
  C : Collection
  where
  C.SubSequence.Index : BidirectionalIndex
>(c: C) {}

func useCollectionTypeSubSequenceGeneratorElement<
  C : Collection
  where
  C.SubSequence.Iterator.Element == C.Iterator.Element
>(c: C) {}

func sortResultIgnored<
  S : Sequence, MC : MutableCollection
  where
  S.Iterator.Element : Comparable,
  MC.Iterator.Element : Comparable
>(
  sequence: S,
  mutableCollection: MC,
  array: [Int]
) {
  var sequence = sequence // expected-warning {{was never mutated; consider changing to 'let' constant}}
  var mutableCollection = mutableCollection // expected-warning {{was never mutated; consider changing to 'let' constant}}
  var array = array // expected-warning {{was never mutated; consider changing to 'let' constant}}

  sequence.sorted() // expected-warning {{result of call to 'sorted()' is unused}}
  sequence.sorted { $0 < $1 } // expected-warning {{result of call to 'sorted' is unused}}

  mutableCollection.sorted() // expected-warning {{result of call to non-mutating function 'sorted()' is unused; use 'sortInPlace()' to mutate in-place}} {{21-27=sortInPlace}}
  mutableCollection.sorted { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sorted' is unused; use 'sortInPlace' to mutate in-place}} {{21-27=sortInPlace}}

  array.sorted() // expected-warning {{result of call to non-mutating function 'sorted()' is unused; use 'sortInPlace()' to mutate in-place}} {{9-15=sortInPlace}}
  array.sorted { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sorted' is unused; use 'sortInPlace' to mutate in-place}} {{9-15=sortInPlace}}
}

struct GoodForwardIndex1 : ForwardIndex {
  func successor() -> GoodForwardIndex1 {
    fatalError("not implemented")
  }
}
func == (lhs: GoodForwardIndex1, rhs: GoodForwardIndex1) -> Bool {
  fatalError("not implemented")
}

struct GoodForwardIndex2 : ForwardIndex {
  func successor() -> GoodForwardIndex2 {
    fatalError("not implemented")
  }
  typealias Distance = Int32
}
func == (lhs: GoodForwardIndex2, rhs: GoodForwardIndex2) -> Bool {
  fatalError("not implemented")
}


struct GoodBidirectionalIndex1 : BidirectionalIndex {
  func successor() -> GoodBidirectionalIndex1 {
    fatalError("not implemented")
  }
  func predecessor() -> GoodBidirectionalIndex1 {
    fatalError("not implemented")
  }
}
func == (lhs: GoodBidirectionalIndex1, rhs: GoodBidirectionalIndex1) -> Bool {
  fatalError("not implemented")
}

struct GoodBidirectionalIndex2 : BidirectionalIndex {
  func successor() -> GoodBidirectionalIndex2 {
    fatalError("not implemented")
  }
  func predecessor() -> GoodBidirectionalIndex2 {
    fatalError("not implemented")
  }
  typealias Distance = Int32
}
func == (lhs: GoodBidirectionalIndex2, rhs: GoodBidirectionalIndex2) -> Bool {
  fatalError("not implemented")
}

// expected-error@+1 {{type 'BadBidirectionalIndex1' does not conform to protocol 'BidirectionalIndex'}}
struct BadBidirectionalIndex1 : BidirectionalIndex {
  func successor() -> BadBidirectionalIndex1 {
    fatalError("not implemented")
  }
  // Missing 'predecessor()'.
}
func == (lhs: BadBidirectionalIndex1, rhs: BadBidirectionalIndex1) -> Bool {
  fatalError("not implemented")
}

struct GoodRandomAccessIndex1 : RandomAccessIndex {
  func successor() -> GoodRandomAccessIndex1 {
    fatalError("not implemented")
  }
  func predecessor() -> GoodRandomAccessIndex1 {
    fatalError("not implemented")
  }
  func distanceTo(other: GoodRandomAccessIndex1) -> Int {
    fatalError("not implemented")
  }
  func advancedBy(n: Int) -> GoodRandomAccessIndex1 {
    fatalError("not implemented")
  }
}
func == (lhs: GoodRandomAccessIndex1, rhs: GoodRandomAccessIndex1) -> Bool {
  fatalError("not implemented")
}

struct GoodRandomAccessIndex2 : RandomAccessIndex {
  func successor() -> GoodRandomAccessIndex2 {
    fatalError("not implemented")
  }
  func predecessor() -> GoodRandomAccessIndex2 {
    fatalError("not implemented")
  }
  func distanceTo(other: GoodRandomAccessIndex2) -> Int32 {
    fatalError("not implemented")
  }
  func advancedBy(n: Int32) -> GoodRandomAccessIndex2 {
    fatalError("not implemented")
  }
  typealias Distance = Int32
}
func == (lhs: GoodRandomAccessIndex2, rhs: GoodRandomAccessIndex2) -> Bool {
  fatalError("not implemented")
}

// expected-error@+2 {{type 'BadRandomAccessIndex1' does not conform to protocol 'RandomAccessIndex'}}
// expected-error@+1 * {{}} // There are a lot of other errors we don't care about.
struct BadRandomAccessIndex1 : RandomAccessIndex {
  func successor() -> BadRandomAccessIndex1 {
    fatalError("not implemented")
  }
  func predecessor() -> BadRandomAccessIndex1 {
    fatalError("not implemented")
  }
}
func == (lhs: BadRandomAccessIndex1, rhs: BadRandomAccessIndex1) -> Bool {
  fatalError("not implemented")
}

// expected-error@+2 {{type 'BadRandomAccessIndex2' does not conform to protocol 'RandomAccessIndex'}}
// expected-error@+1 * {{}} // There are a lot of other errors we don't care about.
struct BadRandomAccessIndex2 : RandomAccessIndex {
  func successor() -> BadRandomAccessIndex2 {
    fatalError("not implemented")
  }
  func predecessor() -> BadRandomAccessIndex2 {
    fatalError("not implemented")
  }
  func distanceTo(other: GoodRandomAccessIndex1) -> Int {
    fatalError("not implemented")
  }
}
func == (lhs: BadRandomAccessIndex2, rhs: BadRandomAccessIndex2) -> Bool {
  fatalError("not implemented")
}

// expected-error@+2 {{type 'BadRandomAccessIndex3' does not conform to protocol 'RandomAccessIndex'}}
// expected-error@+1 * {{}} // There are a lot of other errors we don't care about.
struct BadRandomAccessIndex3 : RandomAccessIndex {
  func successor() -> BadRandomAccessIndex3 {
    fatalError("not implemented")
  }
  func predecessor() -> BadRandomAccessIndex3 {
    fatalError("not implemented")
  }
  func advancedBy(n: Int) -> GoodRandomAccessIndex1 {
    fatalError("not implemented")
  }
}
func == (lhs: BadRandomAccessIndex3, rhs: BadRandomAccessIndex3) -> Bool {
  fatalError("not implemented")
}

