// rdar://177120239
// RUN: %target-typecheck-verify-swift -swift-version 6

// REQUIRES: concurrency

func test() async {
  await withCheckedContinuation { continuation in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    _ = Task { @MainActor in
      return nil as Int?
    }
  }
}

struct Cont<T> {}

func h<T>(_ body: (Cont<T>) -> Void) -> T { fatalError() }
func h<T>(x: Int, _ body: (Cont<T>) -> Void) -> T { fatalError() }

func g() {
  h { c in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    let b = c
  }
}

func k<T>() -> T { fatalError() }
func k<T>(x: Int) -> T { fatalError() }

func f() {
  k() // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}

func download(completion: @escaping (Error?) -> Void) {}

func testDownload() async throws {
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
