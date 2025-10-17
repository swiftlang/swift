// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -typecheck /dev/null -verify

// REQUIRES: concurrency

enum MyError: Error {
case exploded
}

func testTypedThrowsContinuations() async throws(MyError) { 
  let _: Int = try await withUnsafeThrowingContinuation { (continuation: UnsafeContinuation<Int, MyError>) in
    continuation.resume(throwing: .exploded)
  }

  let _: Int = try await withCheckedThrowingContinuation { (continuation: CheckedContinuation<Int, MyError>) in
    continuation.resume(throwing: .exploded)
  }
}
