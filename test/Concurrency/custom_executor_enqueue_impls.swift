// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -o /dev/null -verify -strict-concurrency=targeted %s
// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -o /dev/null -verify -strict-concurrency=complete %s

// REQUIRES: concurrency

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// Such type may be encountered since Swift 5.5 (5.1 backdeployed) if someone implemented the
// not documented, but public Executor types back then already.
//
// We keep support for them, but also log a deprecation warning that they should move to the new signature.
final class OldExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {} // no warning, we're not deprecating the UnownedJob enqueue method yet

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Implementing both enqueue methods is legal, but somewhat useless --
/// we call into the "old one"; so the Owned version is not used in such impl.
///
/// That's why we do log the deprecation warning, people should use the move-only version.
final class BothExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {} // no warning, we're not deprecating the UnownedJob enqueue method yet

  func enqueue(_ job: __owned ExecutorJob) {}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// For now we must keep all 3 implementation kinds and warn about deprecated ones
final class TripleExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {} // no warning, we're not deprecating the UnownedJob enqueue method yet

  // expected-warning@+2{{'Job' is deprecated: renamed to 'ExecutorJob'}}
  // expected-note@+1{{use 'ExecutorJob' instead}}
  func enqueue(_ job: __owned Job) {} // expected-warning{{'Executor.enqueue(Job)' is deprecated as a protocol requirement; conform type 'TripleExecutor' to 'Executor' by implementing 'func enqueue(ExecutorJob)' instead}}{{documentation-file=deprecated-declaration}}

  func enqueue(_ job: consuming ExecutorJob) {}

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

/// Job still is deprecated
final class StillDeprecated: SerialExecutor {
  // expected-warning@+2{{'Job' is deprecated: renamed to 'ExecutorJob'}}
  // expected-note@+1{{use 'ExecutorJob' instead}}
  func enqueue(_ job: __owned Job) {} // expected-warning{{'Executor.enqueue(Job)' is deprecated as a protocol requirement; conform type 'StillDeprecated' to 'Executor' by implementing 'func enqueue(ExecutorJob)' instead}}{{documentation-file=deprecated-declaration}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Just implementing the new signature causes no warnings, good.
final class NewExecutor: SerialExecutor {
  func enqueue(_ job: consuming ExecutorJob) {} // no warnings

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

// Good impl, but missing the ownership keyword
final class MissingOwnership: SerialExecutor { // expected-error {{type 'MissingOwnership' does not conform to protocol 'Executor'}}
  func enqueue(_ job: ExecutorJob) {} // expected-error{{parameter of noncopyable type 'ExecutorJob' must specify ownership}}
  // expected-note@-1{{add 'borrowing' for an immutable reference}}
  // expected-note@-2{{add 'inout' for a mutable reference}}
  // expected-note@-3{{add 'consuming' to take the value from the caller}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}
