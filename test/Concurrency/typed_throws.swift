// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

enum MyError: Error {
  case failed
  case epicFailed
}

@available(SwiftStdlib 6.0, *)
func testAsyncFor<S: AsyncSequence>(seq: S) async throws(MyError) {
  // expected-error@+1{{thrown expression type 'S.Failure' cannot be converted to error type 'MyError'}}
  for try await _ in seq {
  }
}

@available(SwiftStdlib 6.0, *)
func testAsyncFor<S: AsyncSequence>(seq: S) async throws(MyError)
  where S.Failure == MyError
{
  for try await _ in seq {
  }
}

do {
  struct Test {
    func compute(_: () async throws -> Void) async rethrows {} // expected-note {{found this candidate}}
    func compute<E>(_: () throws(E) -> Void) throws(E) {} // expected-note {{found this candidate}}
  }

  func test(_: @Sendable (Test) async throws -> Void) {}

  func unrelated() async {}

  // The solutions are incomparable because rethrows version cannot be called through typed throws one.
  test {
    await unrelated()
    $0.compute {} // expected-error {{ambiguous use of 'compute'}}
  }
}
