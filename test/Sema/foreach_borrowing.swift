// RUN: %target-typecheck-verify-swift \
// RUN:     -enable-experimental-feature BorrowingForLoop \
// RUN:     -enable-experimental-feature BorrowingSequence \
// RUN:     -verify-ignore-unrelated

// REQUIRES: swift_feature_BorrowingForLoop
// REQUIRES: swift_feature_BorrowingSequence

@available(SwiftStdlib 6.4, *)
struct DualConformanceSeq: Sequence, Iterable {
  typealias Element = Int
  typealias IterableIterator = IterableIteratorAdapter<IndexingIterator<[Int]>>
  func makeIterator() -> IndexingIterator<[Int]> { return [].makeIterator() }
}

@available(SwiftStdlib 6.4, *)
func testSequencePreferredOverIterable(seq: DualConformanceSeq) {
  for x in seq {
    _ = x
  }
}

// A Iterable conformance that is incomplete because the declared
// IterableIterator type does not conform to IterableIteratorProtocol.
// The type also conforms to Sequence.
@available(SwiftStdlib 6.4, *)
struct IncompleteBorrowingWithSequence: Sequence, Iterable { // expected-error {{type 'IncompleteBorrowingWithSequence' does not conform to protocol 'Iterable'}} expected-note {{add stubs for conformance}}
  typealias Element = Int
  typealias IterableIterator = Int // expected-note {{possibly intended match 'IncompleteBorrowingWithSequence.IterableIterator' (aka 'Int') does not conform to 'IterableIteratorProtocol'}}
  func makeIterator() -> IndexingIterator<[Int]> { return [].makeIterator() }
}

@available(SwiftStdlib 6.4, *)
func testIncompleteBorrowingWithSequence(seq: IncompleteBorrowingWithSequence) {
  for x in seq {
    _ = x
  }
}

// A Iterable conformance that is incomplete because a required member
// (makeIterableIterator) is not provided. The type has no Sequence
// conformance. Because the conformance is still registered, the compiler
// attempts the Iterable desugar path, and fails to infer the element
// type from the incomplete conformance.
@available(SwiftStdlib 6.4, *)
struct IncompleteBorrowingNoSequence: Iterable { // expected-error {{type 'IncompleteBorrowingNoSequence' does not conform to protocol 'Iterable'}} expected-note {{add stubs for conformance}}
  typealias Element = Int
  typealias IterableIterator = SpanIterator<Int>
  // makeIterableIterator() intentionally omitted
}

@available(SwiftStdlib 6.4, *)
func testIncompleteBorrowingNoSequence(seq: IncompleteBorrowingNoSequence) {
  for x in seq { // expected-error {{generic parameter 'Element' could not be inferred}}
    _ = x
  }
}
