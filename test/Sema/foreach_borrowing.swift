// RUN: %target-typecheck-verify-swift \
// RUN:     -enable-experimental-feature BorrowingForLoop \
// RUN:     -enable-experimental-feature BorrowingSequence \
// RUN:     -verify-ignore-unrelated

// REQUIRES: swift_feature_BorrowingForLoop
// REQUIRES: swift_feature_BorrowingSequence

@available(SwiftStdlib 6.4, *)
struct DualConformanceSeq: Sequence, BorrowingSequence {
  typealias Element = Int
  typealias BorrowingIterator = BorrowingIteratorAdapter<IndexingIterator<[Int]>>
  func makeIterator() -> IndexingIterator<[Int]> { return [].makeIterator() }
}

@available(SwiftStdlib 6.4, *)
func testSequencePreferredOverBorrowingSequence(seq: DualConformanceSeq) {
  for x in seq {
    _ = x
  }
}

// A BorrowingSequence conformance that is incomplete because the declared
// BorrowingIterator type does not conform to BorrowingIteratorProtocol.
// The type also conforms to Sequence.
@available(SwiftStdlib 6.4, *)
struct IncompleteBorrowingWithSequence: Sequence, BorrowingSequence { // expected-error {{type 'IncompleteBorrowingWithSequence' does not conform to protocol 'BorrowingSequence'}} expected-note {{add stubs for conformance}}
  typealias Element = Int
  typealias BorrowingIterator = Int // expected-note {{possibly intended match 'IncompleteBorrowingWithSequence.BorrowingIterator' (aka 'Int') does not conform to 'BorrowingIteratorProtocol'}}
  func makeIterator() -> IndexingIterator<[Int]> { return [].makeIterator() }
}

@available(SwiftStdlib 6.4, *)
func testIncompleteBorrowingWithSequence(seq: IncompleteBorrowingWithSequence) {
  for x in seq {
    _ = x
  }
}

// A BorrowingSequence conformance that is incomplete because a required member
// (makeBorrowingIterator) is not provided. The type has no Sequence
// conformance. Because the conformance is still registered, the compiler
// attempts the BorrowingSequence desugar path, and fails to infer the element
// type from the incomplete conformance.
@available(SwiftStdlib 6.4, *)
struct IncompleteBorrowingNoSequence: BorrowingSequence { // expected-error {{type 'IncompleteBorrowingNoSequence' does not conform to protocol 'BorrowingSequence'}} expected-note {{add stubs for conformance}}
  typealias Element = Int
  typealias BorrowingIterator = SpanIterator<Int>
  // makeBorrowingIterator() intentionally omitted
}

@available(SwiftStdlib 6.4, *)
func testIncompleteBorrowingNoSequence(seq: IncompleteBorrowingNoSequence) {
  for x in seq { // expected-error {{generic parameter 'Element' could not be inferred}}
    _ = x
  }
}
