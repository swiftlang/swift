// RUN: %target-typecheck-verify-swift \
// RUN:     -enable-experimental-feature Lifetimes \
// RUN:     -verify-ignore-unrelated

// REQUIRES: swift_feature_Lifetimes

@available(SwiftStdlib 6.4, *)
func testProtocolConstraintThrows<S: Iterable>(
  seq: borrowing S
) throws(S.Failure) where S.Element == Int {
  for try element in seq {
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testProtocolConstraintInDoCatch<S: Iterable>(
  seq: borrowing S
) where S.Element == Int {
  do {
    for try element in seq {
      _ = element
    }
  } catch {
    _ = error
  }
}

@available(SwiftStdlib 6.4, *)
func testProtocolConstraintMismatch<S: Iterable>(
  seq: borrowing S
) throws(OtherError) where S.Element == Int, S.Failure == IterationError {
  for try element in seq { // expected-error {{thrown expression type 'IterationError' cannot be converted to error type 'OtherError'}}
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testProtocolConstraintNeverFailure<S: Iterable>(
  seq: borrowing S
) where S.Element == Int, S.Failure == Never {
  for element in seq {
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testProtocolConstraintMissingTry<S: Iterable>(
  seq: borrowing S
) where S.Element == Int, S.Failure == IterationError {
  for element in seq { // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
    _ = element
  }
}

enum IterationError: Error {
  case exhausted
}

enum OtherError: Error {
  case other
}

// A throwing BorrowingIteratorProtocol conformer with Failure != Never.
@available(SwiftStdlib 6.4, *)
struct ThrowingSpanIterator: BorrowingIteratorProtocol, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = IterationError

  var _inner: Span<Int>.BorrowingIterator

  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func nextSpan(maxCount: Int) throws(IterationError) -> Span<Int> {
    _inner.nextSpan(maxCount: maxCount)
  }
}

// A throwing Iterable conformer.
@available(SwiftStdlib 6.4, *)
struct ThrowingBorrowingSeq: Iterable, ~Copyable, ~Escapable {
  typealias Element = Int
  typealias Failure = IterationError
  typealias BorrowingIterator = ThrowingSpanIterator

  var _span: Span<Int>

  @_lifetime(borrow self)
  func makeBorrowingIterator() -> ThrowingSpanIterator {
    ThrowingSpanIterator(_inner: .init(_span))
  }
}

@available(SwiftStdlib 6.4, *)
func testMissingTry(seq: borrowing ThrowingBorrowingSeq) {
  for element in seq { // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testForTryInThrowsFunc(seq: borrowing ThrowingBorrowingSeq) throws {
  for try element in seq {
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testForInDoCatch(seq: borrowing ThrowingBorrowingSeq) {
  do {
    for try element in seq {
      _ = element
    }
  } catch {
    _ = error
  }
}

@available(SwiftStdlib 6.4, *)
func testForTryInDoCatch(seq: borrowing ThrowingBorrowingSeq) {
  do {
    for try element in seq {
      _ = element
    }
  } catch {
    _ = error
  }
}

@available(SwiftStdlib 6.4, *)
func testForTryUnhandled(seq: borrowing ThrowingBorrowingSeq) {
  for try element in seq { // expected-error {{errors thrown from here are not handled}}
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testNonThrowingNoTryNeeded(seq: borrowing Span<Int>) {
  for element in seq {
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testTypedThrowsMismatch(seq: borrowing ThrowingBorrowingSeq) throws(OtherError) {
  for try element in seq { // expected-error {{thrown expression type 'IterationError' cannot be converted to error type 'OtherError'}}
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testForTryWithWhereClause(seq: borrowing ThrowingBorrowingSeq) throws(IterationError) {
  for try element in seq where element > 0 {
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testForTryWithWhereClauseMissingTry(seq: borrowing ThrowingBorrowingSeq) {
  for element in seq where element > 0 { // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testTypedThrowsWithAnyErrorFunc(seq: borrowing ThrowingBorrowingSeq) throws {
  for try element in seq {
    _ = element
  }
}

@available(SwiftStdlib 6.4, *)
func testNeverFailureNoTryNeeded(seq: borrowing Span<Int>) throws(IterationError) {
  for element in seq {
    _ = element
  }
}
