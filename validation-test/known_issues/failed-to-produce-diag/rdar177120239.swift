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
