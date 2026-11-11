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
