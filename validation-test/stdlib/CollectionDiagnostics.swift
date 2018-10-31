// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

import StdlibUnittest
import StdlibCollectionUnittest

//
// Check that Collection.SubSequence is constrained to Collection.
//

// expected-error@+2 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'Sequence'}}
// expected-error@+1 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'Collection'}}
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

  // expected-note@+1 {{possibly intended match}}
  typealias SubSequence = OpaqueValue<Int8>
}

func useCollectionTypeSubSequenceIndex<C : Collection>(_ c: C) {}

func useCollectionTypeSubSequenceGeneratorElement<C : Collection>(_ c: C) {}

func sortResultIgnored<
  S : Sequence,
  MC : MutableCollection
>(_ sequence: S, mutableCollection: MC, array: [Int])
  where S.Iterator.Element : Comparable, MC.Iterator.Element : Comparable {
  var sequence = sequence // expected-warning {{variable 'sequence' was never mutated; consider changing to 'let' constant}}
  var mutableCollection = mutableCollection // expected-warning {{variable 'mutableCollection' was never mutated; consider changing to 'let' constant}}
  var array = array // expected-warning {{variable 'array' was never mutated; consider changing to 'let' constant}}

  sequence.sorted() // expected-warning {{result of call to 'sorted()' is unused}}
  sequence.sorted { $0 < $1 } // expected-warning {{result of call to 'sorted(by:)' is unused}}

  mutableCollection.sorted() // expected-warning {{result of call to 'sorted()' is unused}}
  mutableCollection.sorted { $0 < $1 } // expected-warning {{result of call to 'sorted(by:)' is unused}}

  array.sorted() // expected-warning {{result of call to 'sorted()' is unused}}
  array.sorted { $0 < $1 } // expected-warning {{result of call to 'sorted(by:)' is unused}}
}

// expected-warning@+2 {{'Indexable' is deprecated: renamed to 'Collection'}}
// expected-note@+1 {{use 'Collection' instead}}
struct GoodIndexable : Indexable { 
  func index(after i: Int) -> Int { return i + 1 }
  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }

  subscript(pos: Int) -> Int { return 0 }
  subscript(bounds: Range<Int>) -> ArraySlice<Int> { return [] }
}


// expected-warning@+2 {{'Indexable' is deprecated: renamed to 'Collection'}}
// expected-note@+1 {{use 'Collection' instead}}
struct AnotherGoodIndexable1 : Indexable {
  func index(after i: Int) -> Int { return i + 1 }
  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }

  subscript(pos: Int) -> Int { return 0 }
}

// expected-warning@+3 {{'Indexable' is deprecated: renamed to 'Collection'}}
// expected-error@+2 {{type 'BadIndexable2' does not conform to protocol 'Collection'}}
// expected-note@+1 {{use 'Collection' instead}}
struct BadIndexable2 : Indexable {
  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }

  subscript(pos: Int) -> Int { return 0 }
  subscript(bounds: Range<Int>) -> ArraySlice<Int> { return [] }
  // Missing index(after:) -> Int
}

// expected-warning@+2 {{'BidirectionalIndexable' is deprecated: renamed to 'BidirectionalCollection'}}
// expected-note@+1 {{use 'BidirectionalCollection' instead}}
struct GoodBidirectionalIndexable1 : BidirectionalIndexable {
  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }
  func index(after i: Int) -> Int { return i + 1 }
  func index(before i: Int) -> Int { return i - 1 }

  subscript(pos: Int) -> Int { return 0 }
  subscript(bounds: Range<Int>) -> ArraySlice<Int> { return [] }
}

// We'd like to see: {{type 'BadBidirectionalIndexable' does not conform to protocol 'BidirectionalIndexable'}}
// But the compiler doesn't generate that error.
// expected-warning@+2 {{'BidirectionalIndexable' is deprecated: renamed to 'BidirectionalCollection'}}
// expected-note@+1 {{use 'BidirectionalCollection' instead}}
struct BadBidirectionalIndexable : BidirectionalIndexable {
  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }

  subscript(pos: Int) -> Int { return 0 }
  subscript(bounds: Range<Int>) -> ArraySlice<Int> { return [] }

  // This is a poor error message; it would be better to get a message
  // that index(before:) was missing.
  //
  // expected-error@+1 {{'index(after:)' has different argument labels from those required by protocol 'BidirectionalCollection' ('index(before:)'}}
  func index(after i: Int) -> Int { return 0 }
}

//
// Check that RangeReplaceableCollection.SubSequence is defaulted.
//

struct RangeReplaceableCollection_SubSequence_IsDefaulted : RangeReplaceableCollection {
  var startIndex: Int { fatalError() }
  var endIndex: Int { fatalError() }

  subscript(pos: Int) -> Int { return 0 }

  func index(after: Int) -> Int { fatalError() }
  func index(before: Int) -> Int { fatalError() }
  func index(_: Int, offsetBy: Int) -> Int { fatalError() }
  func distance(from: Int, to: Int) -> Int { fatalError() }

  mutating func replaceSubrange<C>(
    _ subrange: Range<Int>,
    with newElements: C
  ) where C : Collection, C.Iterator.Element == Int {
    fatalError()
  }
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: possibly intended match
// <unknown>:0: error: unexpected note produced: possibly intended match
