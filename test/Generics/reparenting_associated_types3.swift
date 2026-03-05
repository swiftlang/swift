// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting
// RUN: %target-swift-frontend %s -emit-irgen -DFIXED -enable-experimental-feature Reparenting > /dev/null

// REQUIRES: swift_feature_Reparenting

// This test exercises the extra witness checking needed for reparented conformances.
// We need to ensure that an abstract witness for an associatedtype requirement from the new parent protocol
// doesn't refer to itself, e.g.,
// "CheckReparentingWitnesses found illegal witness Self.Element of BorrowingSeq that is not within expected protocol context Seq"

public protocol Seq: BorrowingSeq {

// Seq should've declared an Element to witness BorrowingSeq's Element
#if FIXED
  associatedtype Element
#endif

  associatedtype Iterator: Iterable
  func makeIterator() -> Iterator

  associatedtype BorrowingSeqIter: BorrowingIter<Element> = Adapter<Iterator>
}

@reparentable
public protocol BorrowingSeq<Element> {
  associatedtype Element  // expected-note {{protocol requires nested type 'Element'}}
  associatedtype BorrowingSeqIter: BorrowingIter<Element> // expected-note {{protocol requires nested type 'BorrowingSeqIter'}}

  func makeBorrowingSeqIter() -> BorrowingSeqIter
}

extension Seq : @reparented BorrowingSeq // expected-error {{type 'Self' does not conform to protocol 'BorrowingSeq'}} // expected-note {{add stubs for conformance}}
    where BorrowingSeqIter == Adapter<Iterator> {
  public func makeBorrowingSeqIter() -> BorrowingSeqIter {
    return Adapter(makeIterator())
  }
}

public struct Adapter<Iterator: Iterable>: BorrowingIter {
  public typealias Element = Iterator.Element
  var iterator: Iterator
  public init(_ iterator: Iterator) { self.iterator = iterator }

  public mutating func nextThing() -> Element? {
    print("Adapter invoking iterator.next()")
    return iterator.next()!
  }
}

public protocol BorrowingIter<Element> {
  associatedtype Element
  mutating func nextThing() -> Element?
}

public protocol Iterable {
  associatedtype Element
  mutating func next() -> Element?
}
