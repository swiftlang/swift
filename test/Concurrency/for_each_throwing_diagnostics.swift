// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency
// REQUIRES: asserts

struct MyError: Error {
  let code: Int
}

@available(SwiftStdlib 5.1, *)
struct ThrowingSeq: AsyncSequence, AsyncIteratorProtocol {
  typealias Element = Int
  mutating func next() async throws -> Int? { nil }
  func makeAsyncIterator() -> Self { self }
}

@available(SwiftStdlib 5.1, *)
struct TypedThrowingSeq: AsyncSequence, AsyncIteratorProtocol {
  typealias Element = Int
  typealias Failure = MyError
  mutating func next() async throws(MyError) -> Int? { nil }
  func makeAsyncIterator() -> Self { self }
}

// Default case: 'for await' in a non-throwing async function.
@available(SwiftStdlib 5.1, *)
func defaultCase(_ seq: ThrowingSeq) async {
  for await _ in seq { }
  // expected-error@-1 {{errors thrown from 'for-in' loop are not handled}} {{6-6= try}}
}

// 'try' is already present but the context cannot handle throws.
@available(SwiftStdlib 5.1, *)
func tryButNoThrows(_ seq: ThrowingSeq) async {
  for try await _ in seq { }
  // expected-error@-1 {{errors thrown from here are not handled}}
}

@available(SwiftStdlib 5.1, *)
func properlyHandled(_ seq: ThrowingSeq) async throws {
  for try await _ in seq { } // no diagnostic
}

@available(SwiftStdlib 5.1, *)
func rethrowsContext(
  _ seq: ThrowingSeq, _ body: () throws -> Void
) async rethrows {
  try body()
  for await _ in seq { }
  // expected-error@-1 {{errors thrown from 'for-in' loop are not handled; a function declared 'rethrows' may only throw if its parameter does}} {{6-6= try}}
}

@available(SwiftStdlib 5.1, *)
func nonExhaustiveCatch(_ seq: TypedThrowingSeq) async {
  do {
    for await _ in seq { }
    // expected-error@-1 {{errors thrown from 'for-in' loop are not handled because the enclosing catch is not exhaustive}} {{8-8= try}}
  } catch let e where e.code == 0 {}
}

// 'defer' body in an async context.
@available(SwiftStdlib 5.1, *)
func deferContext(_ seq: ThrowingSeq) async {
  defer {
    for await _ in seq { }
    // expected-error@-1 {{errors thrown from 'for-in' loop cannot be thrown out of a defer body}}
  }
  _ = seq
}

// 'try' missing in a context that handles throws.
@available(SwiftStdlib 5.1, *)
func missingTryHandled(_ seq: ThrowingSeq) async throws {
  for await _ in seq { }
  // expected-error@-1 {{'for-in' loop can throw, but is not marked with 'try'}} {{6-6= try}}
}