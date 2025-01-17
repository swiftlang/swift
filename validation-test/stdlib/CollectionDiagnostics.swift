// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

import StdlibUnittest
import StdlibCollectionUnittest

//
// Check that Collection.SubSequence is constrained to Collection.
//

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

// expected-warning@+4 {{'Indexable' is deprecated: renamed to 'Collection'}}
// expected-error@+3 {{type 'BadIndexable2' does not conform to protocol 'Collection'}}
// expected-note@+2 {{use 'Collection' instead}}
// expected-note@+1 {{add stubs for conformance}}
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

//
// A Collection that does not use `Slice<Self>` as its SubSequence should
// require its own implementation of the Range<Index> subscript getter.
// The only valid default implementation of that Collection requirement
// returns `Slice<Self>`.
//

// expected-error@+2 {{type 'CollectionWithNonDefaultSubSequence' does not conform to protocol 'Collection'}}
// expected-error@+1 {{unavailable subscript 'subscript(_:)' was used to satisfy a requirement of protocol 'Collection'}}
struct CollectionWithNonDefaultSubSequence: Collection {
  public var startIndex: Int
  public var endIndex: Int

  public typealias SubSequence = Self

  public func index(after i: Int) -> Int { i+1 }
  public subscript(position: Int) -> Int { position }
}

// expected-error@+2 {{type 'MutableCollectionWithNonDefaultSubSequence' does not conform to protocol 'MutableCollection'}}
// expected-error@+1 {{unavailable subscript 'subscript(_:)' was used to satisfy a requirement of protocol 'MutableCollection'}}
struct MutableCollectionWithNonDefaultSubSequence: MutableCollection {
  public var startIndex: Int
  public var endIndex: Int

  public typealias SubSequence = Self

  public func index(after i: Int) -> Int { i+1 }
  public subscript(position: Int) -> Int {
    get { position }
    set { _ = newValue }
  }

  public subscript(bounds: Range<Index>) -> Self {
    Self(startIndex: bounds.startIndex, endIndex: bounds.endIndex)
  }
}

struct MutableCollectionWithDefaultSubSequence: MutableCollection {
  public var startIndex: Int
  public var endIndex: Int

  public func index(after i: Int) -> Int { i+1 }
  public subscript(position: Int) -> Int {
    get { position }
    set { _ = newValue }
  }
}

func subscriptMutableCollectionIgnored() {
  let cs: MutableCollectionWithNonDefaultSubSequence
  cs = .init(startIndex: 0, endIndex: 10)

  let badSlice: Slice<MutableCollectionWithNonDefaultSubSequence>
  badSlice = cs[0..<2] // expected-error {{'subscript(_:)' is unavailable}}
  cs[3..<5] = badSlice // expected-error {{'subscript(_:)' is unavailable}}

  let ds: MutableCollectionWithDefaultSubSequence
  ds = .init(startIndex: 0, endIndex: 10)

  let goodSlice: Slice<MutableCollectionWithDefaultSubSequence>
  goodSlice = ds[0..<2]
  ds[3..<5] = goodSlice
}

// expected-error@+2 {{type 'IncompleteRangeReplaceableCollection' does not conform to protocol 'RangeReplaceableCollection'}}
// expected-error@+1 {{unavailable instance method 'replaceSubrange(_:with:)' was used to satisfy a requirement of protocol 'RangeReplaceableCollection'}}
struct IncompleteRangeReplaceableCollection: RangeReplaceableCollection {
  var startIndex: Int
  var endIndex: Int

  func index(after i: Int) -> Int { i+1 }
  subscript(position: Int) -> Int { position }

  init() { startIndex = 0; endIndex = 0 }
 }

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: possibly intended match
// <unknown>:0: error: unexpected note produced: possibly intended match
