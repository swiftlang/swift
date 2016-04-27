// RUN: %target-parse-verify-swift

import StdlibUnittest
import StdlibCollectionUnittest


//
// Check that Collection.SubSequence is constrained to Collection.
//

// expected-error@+3 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'Collection'}}
// expected-error@+2 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'IndexableBase'}}
// expected-error@+1 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'Sequence'}}
struct CollectionWithBadSubSequence : Collection {
  var startIndex: MinimalIndex {
    fatalError("unreachable")
  }

  var endIndex: MinimalIndex {
    fatalError("unreachable")
  }

  subscript(i: MinimalIndex) -> OpaqueValue<Int> {
    fatalError("unreachable")
  }

  // expected-note@+3 {{possibly intended match 'SubSequence' (aka 'OpaqueValue<Int8>') does not conform to 'IndexableBase'}}
  // expected-note@+2 {{possibly intended match}}
  // expected-note@+1 {{possibly intended match}}
  typealias SubSequence = OpaqueValue<Int8>
}

func useCollectionTypeSubSequenceIndex<
  C : Collection
  where
  C.SubSequence.Index == C.Index
>(_ c: C) {}

func useCollectionTypeSubSequenceGeneratorElement<
  C : Collection
  where
  C.SubSequence.Iterator.Element == C.Iterator.Element
>(_ c: C) {}

func sortResultIgnored<
  S : Sequence, MC : MutableCollection
  where
  S.Iterator.Element : Comparable,
  MC.Iterator.Element : Comparable
>(
  _ sequence: S,
  mutableCollection: MC,
  array: [Int]
) {
  var sequence = sequence // expected-warning {{variable 'sequence' was never mutated; consider changing to 'let' constant}}
  var mutableCollection = mutableCollection // expected-warning {{variable 'mutableCollection' was never mutated; consider changing to 'let' constant}}
  var array = array // expected-warning {{variable 'array' was never mutated; consider changing to 'let' constant}}

  sequence.sorted() // expected-warning {{result of call to 'sorted()' is unused}}
  sequence.sorted { $0 < $1 } // expected-warning {{result of call to 'sorted(isOrderedBefore:)' is unused}}

  mutableCollection.sorted() // expected-warning {{result of call to non-mutating function 'sorted()' is unused; use 'sort()' to mutate in-place}} {{21-27=sort}}
  mutableCollection.sorted { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sorted(isOrderedBefore:)' is unused; use 'sort(isOrderedBefore:)' to mutate in-place}} {{21-27=sort}}

  array.sorted() // expected-warning {{result of call to non-mutating function 'sorted()' is unused; use 'sort()' to mutate in-place}} {{9-15=sort}}
  array.sorted { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sorted(isOrderedBefore:)' is unused; use 'sort(isOrderedBefore:)' to mutate in-place}} {{9-15=sort}}
}

struct GoodIndexable : Indexable {
  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }

  subscript(pos: Int) -> Int { return 0 }
  subscript(bounds: Range<Int>) -> [Int] { return [] }
}


// expected-error@+1 {{type 'BadIndexable1' does not conform to protocol 'IndexableBase'}}
struct BadIndexable1 : Indexable {
  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }

  subscript(pos: Int) -> Int { return 0 }

  // Missing 'subscript(_:) -> SubSequence'.
}

// expected-error@+1 {{type 'BadIndexable2' does not conform to protocol 'IndexableBase'}}
struct BadIndexable2 : Indexable {
  var startIndex: String { return "" }
  var endIndex: String { return "" }

  subscript(pos: String) -> Int { return 0 }
  subscript(bounds: Range<String>) -> [Int] { return [] }

  // Since String is not Strideable, 'index(after:)' will not be defaulted
}

struct GoodBidirectionalIndexable1 : BidirectionalIndexable {
  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }

  subscript(pos: Int) -> Int { return 0 }
  subscript(bounds: Range<Int>) -> [Int] { return [] }

  // all the methods have default implementations in extensions
}

struct GoodBidirectionalIndexable2 : BidirectionalIndexable {
  var startIndex: String { return "" }
  var endIndex: String { return "" }

  subscript(pos: String) -> Int { return 0 }
  subscript(bounds: Range<String>) -> [Int] { return [] }

  func index(after i: String) -> String { return "" }
  func index(before i: String) -> String { return "" }
}

// expected-error@+1 {{type 'BadBidirectionalIndexable' does not conform to protocol 'BidirectionalIndexable'}}
struct BadBidirectionalIndexable : BidirectionalIndexable {
  var startIndex: String { return "" }
  var endIndex: String { return "" }

  subscript(pos: String) -> Int { return 0 }
  subscript(bounds: Range<String>) -> [Int] { return [] }

  func index(after i: String) -> String { return "" }

  // Since String is not Strideable, 'index(before:)' will not be defaulted
}
