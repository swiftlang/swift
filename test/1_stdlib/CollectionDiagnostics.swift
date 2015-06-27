// RUN: %target-parse-verify-swift

import StdlibUnittest

//
// Check that CollectionType._prext_SubSlice is constrained to CollectionType.
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

  // expected-note@+1 {{possibly intended match '_prext_SubSequence' does not conform to '_prext_Indexable'}}
  typealias _prext_SubSequence = OpaqueValue<Int8>
}

func useCollectionTypeSubSequenceIndex<
  C : CollectionType
  where
  C._prext_SubSequence.Index : BidirectionalIndexType
>(c: C) {}

func useCollectionTypeSubSequenceGeneratorElement<
  C : CollectionType
  where
  C._prext_SubSequence.Generator.Element == C.Generator.Element
>(c: C) {}

func sortResultIgnored<
  S : SequenceType, MC : MutableCollectionType
  where
  S.Generator.Element : Comparable,
  MC.Generator.Element : Comparable
>(
  var sequence: S, // expected-warning {{parameter 'sequence' was never mutated; consider changing to 'let' constant}}
  var mutableCollection: MC, // expected-warning {{parameter 'mutableCollection' was never mutated; consider changing to 'let' constant}}
  var array: [Int] // expected-warning {{parameter 'array' was never mutated; consider changing to 'let' constant}}
) {

  sequence.sort() // expected-warning {{result of call to 'sort()' is unused}}
  sequence.sort { $0 < $1 } // expected-warning {{result of call to 'sort' is unused}}

  mutableCollection.sort() // expected-warning {{result of call to non-mutating function 'sort()' is unused; use 'sortInPlace()' to mutate in-place}}
  mutableCollection.sort { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sort' is unused; use 'sortInPlace' to mutate in-place}}

  array.sort() // expected-warning {{result of call to non-mutating function 'sort()' is unused; use 'sortInPlace()' to mutate in-place}}
  array.sort { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sort' is unused; use 'sortInPlace' to mutate in-place}}
}

extension Array {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension ArraySlice {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension ContiguousArray {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension Set {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension SetGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension SetIndex {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension Repeat {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension GeneratorOfOne {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension CollectionOfOne {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension EmptyGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension EmptyCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension UnsafeBufferPointerGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension UnsafeBufferPointer {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension UnsafeMutableBufferPointer {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension Range {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension RangeGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension StrideToGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension StrideTo {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension StrideThroughGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension StrideThrough {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension AnyGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension AnySequence {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension AnyForwardCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension AnyBidirectionalCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension AnyRandomAccessCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension FilterSequenceView {} // expected-error {{'FilterSequenceView' has been renamed to 'FilterSequence'}}
extension FilterCollectionViewIndex {} // expected-error {{'FilterCollectionViewIndex' has been renamed to 'FilterCollectionIndex'}}
extension FilterCollectionView {} // expected-error {{'FilterCollectionView' has been renamed to 'FilterCollection'}}

extension MapGenerator {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension MapSequence {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}
extension MapCollection {
  func foo(element: T) {} // expected-error {{'T' has been renamed to 'Element'}}
}

extension MapSequenceGenerator {} // expected-error {{'MapSequenceGenerator' has been renamed to 'MapGenerator'}}
extension MapSequenceView {} // expected-error {{'MapSequenceView' has been renamed to 'MapSequence'}}
extension MapCollectionView {} // expected-error {{'MapCollectionView' has been renamed to 'MapCollection'}}

extension LazySequence {
  func foo(base: S) {} // expected-error {{'S' has been renamed to 'Base'}}
}
extension LazyForwardCollection {
  func foo(base: S) {} // expected-error {{'S' has been renamed to 'Base'}}
}
extension LazyBidirectionalCollection {
  func foo(base: S) {} // expected-error {{'S' has been renamed to 'Base'}}
}
extension LazyRandomAccessCollection {
  func foo(base: S) {} // expected-error {{'S' has been renamed to 'Base'}}
}

extension ReverseIndex {
  func foo(base: I) {} // expected-error {{'I' has been renamed to 'Base'}}
}
extension ReverseRandomAccessIndex {
  func foo(base: I) {} // expected-error {{'I' has been renamed to 'Base'}}
}
extension ReverseCollection {
  func foo(base: T) {} // expected-error {{'T' has been renamed to 'Base'}}
}
extension ReverseRandomAccessCollection {
  func foo(base: T) {} // expected-error {{'T' has been renamed to 'Base'}}
}

extension BidirectionalReverseView {} // expected-error {{'BidirectionalReverseView' has been renamed to 'ReverseCollection'}}
extension RandomAccessReverseView {} // expected-error {{'RandomAccessReverseView' has been renamed to 'ReverseRandomAccessCollection'}}

extension GeneratorSequence {
  func foo(base: G) {} // expected-error {{'G' has been renamed to 'Base'}}
}

extension ZipGenerator2 {} // expected-error {{'ZipGenerator2' has been renamed to 'Zip2Generator'}}
extension Zip2 {} // expected-error {{'Zip2' has been renamed to 'Zip2Sequence'}}

extension AutoreleasingUnsafeMutablePointer {
  func foo(memory: T) {} // expected-error {{'T' has been renamed to 'Memory'}}
}
extension UnsafePointer {
  func foo(memory: T) {} // expected-error {{'T' has been renamed to 'Memory'}}
}
extension UnsafeMutablePointer {
  func foo(memory: T) {} // expected-error {{'T' has been renamed to 'Memory'}}
}

extension HalfOpenInterval {
  func foo(bound: T) {} // expected-error {{'T' has been renamed to 'Bound'}}
}
extension ClosedInterval {
  func foo(bound: T) {} // expected-error {{'T' has been renamed to 'Bound'}}
}

extension Unmanaged {
  func foo(instance: T) {} // expected-error {{'T' has been renamed to 'Instance'}}
}

