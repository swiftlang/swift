// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency

// REQUIRES: concurrency

enum MyError : Error {
  case bad
}

func shouldThrow() async {
  // expected-error@+1 {{errors thrown from here are not handled}}
  let _: Int = await try withUnsafeThrowingContinuation { continuation in
    continuation.resume(throwing: MyError.bad)
  }
}
