// RUN: %target-typecheck-verify-swift \
// RUN:     -enable-experimental-feature BorrowingForLoop \
// RUN:     -enable-experimental-feature BorrowingSequence \
// RUN:     -verify-ignore-unrelated

// REQUIRES: swift_feature_BorrowingForLoop
// REQUIRES: swift_feature_BorrowingSequence
// REQUIRES: OS=macosx

// Tests for the availability check in shouldUseBorrowingSequence.
//
// BorrowingSequence is @available(SwiftStdlib 6.4, *). When a for-in loop
// over a BorrowingSequence-conforming type appears in a context whose
// availability does not cover SwiftStdlib 6.4, the compiler falls back to
// treating the sequence as a Sequence. If the type does not also conform to
// Sequence this produces a diagnostic.

// Without @available(SwiftStdlib 6.4, *) on the enclosing function the
// availability context does not cover SwiftStdlib 6.4.
// The compiler tries the Sequence path, and because Span<Int> does not conform 
// to Sequence a diagnostic is emitted.
func testForLoopOverSpanWithoutAvailability(_ seq: Span<Int>) {
  for x in seq { // expected-error {{for-in loop requires 'Span<Int>' to conform to 'Sequence'}}
    _ = x
  }
}

// With @available(SwiftStdlib 6.4, *), the availability context covers the
// protocol, and the loop desugars via the BorrowingSequence path.
@available(SwiftStdlib 6.4, *)
func testForLoopOverSpanWithAvailability(_ seq: Span<Int>) {
  for x in seq {
    _ = x
  }
}

// Inside an if #available(SwiftStdlib 6.4, *) block, the availability context
// is also sufficient.
func testForLoopInsideAvailabilityGuard() {
  let arr: [Int] = [0, 1, 2, 3]
  if #available(SwiftStdlib 6.4, *) {
    let seq = arr.span
    for x in seq {
      _ = x
    }
  }
}

// A type that unconditionally conforms to Sequence but whose BorrowingSequence
// conformance is only available under SwiftStdlib 6.4. In a context without
// that availability, the BorrowingSequence conformance is unavailable.
// The loop uses Sequence.
struct BorrowingFallbackWithSequence: Sequence {
  typealias Element = Int
  func makeIterator() -> IndexingIterator<[Int]> { return [].makeIterator() }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingFallbackWithSequence: BorrowingSequence {
  typealias BorrowingIterator = BorrowingIteratorAdapter<IndexingIterator<[Int]>>
}

func testInvalidBorrowingWithSequence(seq: BorrowingFallbackWithSequence) {
  for x in seq {
    _ = x
  }
}

// A type whose only sequence conformance is BorrowingSequence, available under
// SwiftStdlib 6.4. In a context without that availability, the compiler falls
// back to Sequence, finds no conformance, and emits a diagnostic.
struct BorrowingFallbackNoSequence {}

@available(SwiftStdlib 6.4, *)
extension BorrowingFallbackNoSequence: BorrowingSequence {
  typealias Element = Int
  typealias BorrowingIterator = BorrowingIteratorAdapter<IndexingIterator<[Int]>>
  func makeBorrowingIterator() -> BorrowingIteratorAdapter<IndexingIterator<[Int]>> {
    BorrowingIteratorAdapter(iterator: [1, 2, 3].makeIterator())
  }
}

func testInvalidBorrowingNoSequence(seq: BorrowingFallbackNoSequence) {
  for x in seq { // expected-error {{for-in loop requires 'BorrowingFallbackNoSequence' to conform to 'Sequence'}}
    _ = x
  }
}
