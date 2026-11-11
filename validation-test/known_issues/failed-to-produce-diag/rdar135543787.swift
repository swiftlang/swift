// rdar://135543787
// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

final class Animation {}
final class AnimationFile {}

struct AnimationView {
  init(_ loadAnimation: @escaping () async throws -> Animation?) {}
  init(_ loadAnimationFile: @escaping () async throws -> AnimationFile?) {}
}

struct Test {
  var animation: Animation {
    get async throws { Animation() }
  }

  func test() {
    _ = AnimationView { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      try await Task.detached {
        try await animation
      }
    }
  }
}
