// rdar://186709597
// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

func extract(completionHandler: @escaping ((any Error)?) -> Void) -> Int { 0 }

func test(dest: String) async throws {
  let result = try await withCheckedThrowingContinuation { cont in
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
    _ = extract(completionHandler: { error in
      if let error {
        cont.resume(throwing: error)
      } else {
        print("Extracted")
        cont.resume(returning: dest)
      }
    })
  }
}
