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
  C.SubSequence.Generator.Element == C.Generator.Element
>(c: C) {}

func sortResultIgnored<
  S : SequenceType, MC : MutableCollectionType
  where
  S.Generator.Element : Comparable,
  MC.Generator.Element : Comparable
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

extension Array {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension ArraySlice {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension ContiguousArray {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension Set {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension SetGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension SetIndex {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension Repeat {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension GeneratorOfOne {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension CollectionOfOne {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension EmptyGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension EmptyCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension UnsafeBufferPointerGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension UnsafeBufferPointer {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension UnsafeMutableBufferPointer {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension Range {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension RangeGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension StrideToGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension StrideTo {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension StrideThroughGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension StrideThrough {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension AnyGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension AnySequence {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension AnyForwardCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension AnyBidirectionalCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension AnyRandomAccessCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension FilterSequenceView {} // expected-error {{'FilterSequenceView' has been renamed to 'FilterSequence'}} {{11-29=FilterSequence}}
extension FilterCollectionViewIndex {} // expected-error {{'FilterCollectionViewIndex' has been renamed to 'FilterCollectionIndex'}} {{11-36=FilterCollectionIndex}}
extension FilterCollectionView {} // expected-error {{'FilterCollectionView' has been renamed to 'FilterCollection'}} {{11-31=FilterCollection}}

extension LazyMapGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension LazyMapSequence {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}
extension LazyMapCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}} {{21-22=Element}}
}

extension MapSequenceGenerator {} // expected-error {{'MapSequenceGenerator' has been renamed to 'LazyMapGenerator'}} {{11-31=LazyMapGenerator}}
extension MapSequenceView {} // expected-error {{'MapSequenceView' has been renamed to 'LazyMapSequence'}} {{11-26=LazyMapSequence}}
extension MapCollectionView {} // expected-error {{'MapCollectionView' has been renamed to 'LazyMapCollection'}} {{11-28=LazyMapCollection}}

extension LazySequence {
  func foo(base: S) {} // expected-error {{'S' has been renamed to 'Base'}} {{18-19=Base}}
  func bar() { _ = self.array } // expected-error {{please construct an Array from your lazy sequence}}
}
extension LazyCollection {
  func foo(base: S) {} // expected-error {{'S' has been renamed to 'Base'}} {{18-19=Base}}
}

func foo<T>(_:LazyForwardCollection<T>) // expected-error {{'LazyForwardCollection' has been renamed to 'LazyCollection'}} {{15-36=LazyCollection}}
func foo<T>(_:LazyBidirectionalCollection<T>) // expected-error {{'LazyBidirectionalCollection' has been renamed to 'LazyCollection'}} {{15-42=LazyCollection}}
func foo<T>(_:LazyRandomAccessCollection<T>) // expected-error {{'LazyRandomAccessCollection' has been renamed to 'LazyCollection'}} {{15-41=LazyCollection}}

extension ReverseIndex {
  func foo(base: I) {} // expected-error {{'I' has been renamed to 'Base'}} {{18-19=Base}}
}
extension ReverseRandomAccessIndex {
  func foo(base: I) {} // expected-error {{'I' has been renamed to 'Base'}} {{18-19=Base}}
}
extension ReverseCollection {
  func foo(base: T) {} // expected-error {{'T' has been renamed to 'Base'}} {{18-19=Base}}
}
extension ReverseRandomAccessCollection {
  func foo(base: T) {} // expected-error {{'T' has been renamed to 'Base'}} {{18-19=Base}}
}

extension BidirectionalReverseView {} // expected-error {{'BidirectionalReverseView' has been renamed to 'ReverseCollection'}} {{11-35=ReverseCollection}}
extension RandomAccessReverseView {} // expected-error {{'RandomAccessReverseView' has been renamed to 'ReverseRandomAccessCollection'}} {{11-34=ReverseRandomAccessCollection}}

extension GeneratorSequence {
  func foo(base: G) {} // expected-error {{'G' has been renamed to 'Base'}} {{18-19=Base}}
}

extension ZipGenerator2 {} // expected-error {{'ZipGenerator2' has been renamed to 'Zip2Generator'}} {{11-24=Zip2Generator}}
extension Zip2 {} // expected-error {{'Zip2' has been renamed to 'Zip2Sequence'}} {{11-15=Zip2Sequence}}

extension UnsafePointer {
  func foo(memory: T) {} // expected-error {{'T' has been renamed to 'Memory'}} {{20-21=Memory}}
}
extension UnsafeMutablePointer {
  func foo(memory: T) {} // expected-error {{'T' has been renamed to 'Memory'}} {{20-21=Memory}}
}

extension HalfOpenInterval {
  func foo(bound: T) {} // expected-error {{'T' has been renamed to 'Bound'}} {{19-20=Bound}}
}
extension ClosedInterval {
  func foo(bound: T) {} // expected-error {{'T' has been renamed to 'Bound'}} {{19-20=Bound}}
}

extension Unmanaged {
  func foo(instance: T) {} // expected-error {{'T' has been renamed to 'Instance'}} {{22-23=Instance}}
}

struct MyCollection : Sliceable {} // expected-error {{'Sliceable' has been renamed to 'CollectionType'}} {{23-32=CollectionType}}
protocol MyProtocol : Sliceable {} // expected-error {{'Sliceable' has been renamed to 'CollectionType'}} {{23-32=CollectionType}}
func processCollection<E : Sliceable>(e: E) {} // expected-error {{'Sliceable' has been renamed to 'CollectionType'}} {{28-37=CollectionType}}

func renamedRangeReplaceableCollectionTypeMethods(c: DefaultedForwardRangeReplaceableCollection<Int>) {
  var c = c
  c.extend([ 10 ]) // expected-error {{'extend' has been renamed to 'appendContentsOf'}} {{5-11=appendContentsOf}}
  c.splice([ 10 ], atIndex: c.startIndex) // expected-error {{'splice(_:atIndex:)' has been renamed to 'insertContentsOf'}} {{5-11=insertContentsOf}}
}

func renamedAnyGenerator<G : GeneratorType>(g: G) {
  _ = anyGenerator(g) // expected-warning {{'anyGenerator' is deprecated: renamed to 'AnyGenerator'}} expected-note {{use 'AnyGenerator' instead}}
  _ = anyGenerator { 1 } // expected-warning {{'anyGenerator' is deprecated: renamed to 'AnyGenerator'}} expected-note {{use 'AnyGenerator' instead}}
}

