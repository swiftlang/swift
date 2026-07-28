// RUN: %target-typecheck-verify-swift \
// RUN:     -verify-ignore-unrelated

// REQUIRES: OS=macosx

// Tests for the availability check in DesugarForEachStmt.
//
// Iterable is @available(SwiftStdlib 6.4, *). When a for-in loop
// over a Iterable-conforming type appears in a context whose
// availability does not cover SwiftStdlib 6.4, the compiler signals that.

// Without @available(SwiftStdlib 6.4, *) on the enclosing function, the
// availability context does not cover SwiftStdlib 6.4.
func testForLoopOverSpanWithoutAvailability(_ seq: Span<Int>) {
  for x in seq { // expected-error {{for-in loop requires 'Span<Int>' to conform to 'Iterable', which is only available in macOS 27.0 or newer}}
  // expected-note@-2 {{add '@available' attribute to enclosing global function}}
  // expected-note@-2 {{add 'if #available' version check}}
    _ = x
  }
}

// With @available(SwiftStdlib 6.4, *), the availability context covers the
// protocol, and the loop desugars via the Iterable path.
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

// A type that unconditionally conforms to Sequence but whose Iterable
// conformance is only available under SwiftStdlib 6.4. In a context without
// that availability, the Iterable conformance is unavailable.
// The loop uses Sequence.
struct BorrowingFallbackWithSequence: Sequence {
  typealias Element = Int
  func makeIterator() -> IndexingIterator<[Int]> { return [].makeIterator() }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingFallbackWithSequence: Iterable {
  typealias BorrowingIterator = BorrowingIteratorAdapter<IndexingIterator<[Int]>>
}

func testInvalidBorrowingWithSequence(seq: BorrowingFallbackWithSequence) {
  for x in seq {
    _ = x
  }
}

// A type whose only sequence conformance is Iterable, available under
// SwiftStdlib 6.4. In a context without that availability, the compiler falls
// emits an error.
struct BorrowingFallbackNoSequence {}

@available(SwiftStdlib 6.4, *)
extension BorrowingFallbackNoSequence: Iterable {
  typealias Element = Int
  typealias BorrowingIterator = BorrowingIteratorAdapter<IndexingIterator<[Int]>>
  func makeBorrowingIterator() -> BorrowingIteratorAdapter<IndexingIterator<[Int]>> {
    BorrowingIteratorAdapter(iterator: [1, 2, 3].makeIterator())
  }
}

func testInvalidBorrowingNoSequence(seq: BorrowingFallbackNoSequence) {
  for x in seq { // expected-error {{for-in loop requires 'BorrowingFallbackNoSequence' to conform to 'Iterable', which is only available in macOS 27.0 or newer}}
  // expected-note@-2 {{add '@available' attribute to enclosing global function}}
  // expected-note@-2 {{add 'if #available' version check}}
    _ = x
  }
}
