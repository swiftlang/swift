// RUN: %target-parse-verify-swift

import StdlibUnittest

//
// Check that CollectionType.SubSequence is constrained to CollectionType.
//

// expected-error@+2 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'SequenceType'}}
// expected-error@+1 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'CollectionType'}}
struct CollectionWithBadSubSequence : CollectionType {
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
  C : CollectionType
  where
  C.SubSequence.Index : BidirectionalIndexType
>(c: C) {}

func useCollectionTypeSubSequenceGeneratorElement<
  C : CollectionType
  where
  C.SubSequence.Iterator.Element == C.Iterator.Element
>(c: C) {}

func sortResultIgnored<
  S : SequenceType, MC : MutableCollectionType
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

  sequence.sort() // expected-warning {{result of call to 'sort()' is unused}}
  sequence.sort { $0 < $1 } // expected-warning {{result of call to 'sort' is unused}}

  mutableCollection.sort() // expected-warning {{result of call to non-mutating function 'sort()' is unused; use 'sortInPlace()' to mutate in-place}} {{21-25=sortInPlace}}
  mutableCollection.sort { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sort' is unused; use 'sortInPlace' to mutate in-place}} {{21-25=sortInPlace}}

  array.sort() // expected-warning {{result of call to non-mutating function 'sort()' is unused; use 'sortInPlace()' to mutate in-place}} {{9-13=sortInPlace}}
  array.sort { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sort' is unused; use 'sortInPlace' to mutate in-place}} {{9-13=sortInPlace}}
}

struct GoodForwardIndex1 : ForwardIndexType {
  func successor() -> GoodForwardIndex1 {
    fatalError("not implemented")
  }
}
func == (lhs: GoodForwardIndex1, rhs: GoodForwardIndex1) -> Bool {
  fatalError("not implemented")
}

struct GoodForwardIndex2 : ForwardIndexType {
  func successor() -> GoodForwardIndex2 {
    fatalError("not implemented")
  }
  typealias Distance = Int32
}
func == (lhs: GoodForwardIndex2, rhs: GoodForwardIndex2) -> Bool {
  fatalError("not implemented")
}


struct GoodBidirectionalIndex1 : BidirectionalIndexType {
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

struct GoodBidirectionalIndex2 : BidirectionalIndexType {
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

// expected-error@+1 {{type 'BadBidirectionalIndex1' does not conform to protocol 'BidirectionalIndexType'}}
struct BadBidirectionalIndex1 : BidirectionalIndexType {
  func successor() -> BadBidirectionalIndex1 {
    fatalError("not implemented")
  }
  // Missing 'predecessor()'.
}
func == (lhs: BadBidirectionalIndex1, rhs: BadBidirectionalIndex1) -> Bool {
  fatalError("not implemented")
}

struct GoodRandomAccessIndex1 : RandomAccessIndexType {
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

struct GoodRandomAccessIndex2 : RandomAccessIndexType {
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

// expected-error@+2 {{type 'BadRandomAccessIndex1' does not conform to protocol 'RandomAccessIndexType'}}
// expected-error@+1 * {{}} // There are a lot of other errors we don't care about.
struct BadRandomAccessIndex1 : RandomAccessIndexType {
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

// expected-error@+2 {{type 'BadRandomAccessIndex2' does not conform to protocol 'RandomAccessIndexType'}}
// expected-error@+1 * {{}} // There are a lot of other errors we don't care about.
struct BadRandomAccessIndex2 : RandomAccessIndexType {
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

// expected-error@+2 {{type 'BadRandomAccessIndex3' does not conform to protocol 'RandomAccessIndexType'}}
// expected-error@+1 * {{}} // There are a lot of other errors we don't care about.
struct BadRandomAccessIndex3 : RandomAccessIndexType {
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

