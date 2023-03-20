// RUN: %target-typecheck-verify-swift -enable-experimental-move-only -disable-availability-checking
// REQUIRES: concurrency

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// Such type may be encountered since Swift 5.5 (5.1 backdeployed) if someone implemented the
// not documented, but public Executor types back then already.
//
// We keep support for them, but also log a deprecation warning that they should move to the new signature.
final class OldExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {} // expected-warning{{'Executor.enqueue(UnownedJob)' is deprecated as a protocol requirement; conform type 'OldExecutor' to 'Executor' by implementing 'func enqueue(Job)' instead}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Implementing both enqueue methods is legal, but somewhat useless --
/// we call into the "old one"; so the Owned version is not used in such impl.
///
/// That's why we do log the deprecation warning, people should use the move-only version.
final class BothExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {} // expected-warning{{'Executor.enqueue(UnownedJob)' is deprecated as a protocol requirement; conform type 'BothExecutor' to 'Executor' by implementing 'func enqueue(Job)' instead}}
  func enqueue(_ job: __owned Job) {}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Even though we do provide default impls for both enqueue requirements,
/// we manually detect and emit an error if neither of them is implemented.
///
/// We do so because we implement them recursively, so one of them must be implemented basically.
final class NoneExecutor: SerialExecutor { // expected-error{{type 'NoneExecutor' does not conform to protocol 'Executor'}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Just implementing the new signature causes no warnings, good.
final class NewExecutor: SerialExecutor {
  func enqueue(_ job: __owned Job) {} // no warnings

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}