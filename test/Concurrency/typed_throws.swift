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

@available(SwiftStdlib 6.0, *)
func testAsyncLet<V, E: Error>(action: () async throws(E) -> V) async throws(MyError) -> V {
  async let value = try await action()
  // expected-error@+1{{thrown expression type 'E' cannot be converted to error type 'MyError'}}
  return try await value
}

@available(SwiftStdlib 6.0, *)
func testAsyncLet<V>(action: () async throws(MyError) -> V) async throws(MyError) -> V {
  async let value = try await action()
  return try await value
}

@available(SwiftStdlib 6.0, *)
func testAsyncLetForceTry<V, E: Error>(action: () async throws(E) -> V) async -> V {
  async let value = try! await action()
  let result = await value
  return result
}

@available(SwiftStdlib 6.0, *)
func testAsyncLetUntypedThrowsInTypedContext<V>(action: () async throws -> V) async throws(MyError) -> V {
  async let value = try await action()
  // expected-error@+1{{thrown expression type 'any Error' cannot be converted to error type 'MyError'}}
  return try await value
}

@available(SwiftStdlib 6.0, *)
func testAsyncLetTypedRethrows<S: AsyncSequence>(seq: S) async throws(S.Failure) -> Int where S.Element == Int, S: Sendable {
  async let value = seq.reduce(0) { $0 + $1 }
  let result = try await value
  return result
}

@available(SwiftStdlib 6.0, *)
func testAsyncLetRethrowsWithThrowingTransform<S: AsyncSequence>(seq: S, transform: @Sendable (Int, Int) async throws -> Int) async throws(S.Failure) -> Int where S.Element == Int, S: Sendable {
  async let value = seq.reduce(0, transform)
  // expected-error@+1{{thrown expression type 'any Error' cannot be converted to error type 'S.Failure'}}
  return try await value
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
