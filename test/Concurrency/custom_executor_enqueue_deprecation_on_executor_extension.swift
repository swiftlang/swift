// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify -strict-concurrency=targeted %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify -strict-concurrency=complete %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding


extension Executor {
  func enqueue(_ job: UnownedJob) { // no warning, we don't deprecate this just yet. It was deprecated in 5.9.0 but we undid this.
    fatalError()
  }
}

final class NoneExecutor: SerialExecutor {

  // the enqueue from the extension is properly picked up,
  // even though we do warn about deprecation on it in the extension.

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}
