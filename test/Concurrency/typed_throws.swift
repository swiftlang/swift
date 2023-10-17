// RUN: %target-typecheck-verify-swift -enable-experimental-feature TypedThrows

// REQUIRES: concurrency

enum MyError: Error {
  case failed
  case epicFailed
}


@available(SwiftStdlib 5.1, *)
func testAsyncFor<S: AsyncSequence>(seq: S) async throws(MyError) {
  // expected-error@+1{{thrown expression type 'any Error' cannot be converted to error type 'MyError'}}
  for try await _ in seq {
  }
}
