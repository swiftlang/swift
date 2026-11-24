// rdar://182341089
// RUN: %target-typecheck-verify-swift
// REQUIRES: concurrency

func download(completion: @escaping (Error?) -> Void) {}

func test() async throws {
  try await withCheckedThrowingContinuation { continuation in
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
    download { error in
      if let error {
        continuation.resume(throwing: error)
      } else {
        continuation.resume()
      }
    }
  }
}
