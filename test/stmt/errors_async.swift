// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency

// REQUIRES: concurrency

enum MyError : Error {
  case bad
}

@available(SwiftStdlib 5.5, *)
func shouldThrow() async {
  // expected-error@+1 {{errors thrown from here are not handled}}
  let _: Int = try await withUnsafeThrowingContinuation { continuation in
    continuation.resume(throwing: MyError.bad)
  }
}
