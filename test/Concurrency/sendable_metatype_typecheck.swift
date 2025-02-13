// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-experimental-feature StrictSendableMetatypes

// REQUIRES: concurrency
// REQUIRES: swift_feature_StrictSendableMetatypes

// This test checks for typecheck-only diagnostics involving non-sendable
// metatypes.

protocol Q {
  static func g()
}

nonisolated func acceptMeta<T>(_: T.Type) { }

nonisolated func staticCallThroughMeta<T: Q>(_: T.Type) {
  Task.detached {
    T.g()
  }
}

nonisolated func passMeta<T: Q>(_: T.Type) {
  Task.detached {
    acceptMeta(T.self)
  }
}
