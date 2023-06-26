// RUN: %target-typecheck-verify-swift -enable-experimental-move-only
// REQUIRES: concurrency

// rdar://106849189 move-only types should be supported in freestanding mode
// UNSUPPORTED: freestanding

// FIXME: rdar://107112715 test failing on iOS simulator, investigating
// UNSUPPORTED: OS=ios

// If the availability is recent enough, log a deprecation warning to move to the new signature.
@available(SwiftStdlib 5.9, *)
final class OldExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {} // expected-warning{{'Executor.enqueue(UnownedJob)' is deprecated as a protocol requirement; conform type 'OldExecutor' to 'Executor' by implementing 'func enqueue(ExecutorJob)' instead}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Such a type may be encountered since Swift 5.5 (5.1 backdeployed) if someone implemented the
/// not documented, but public Executor types back then already. Allow these to be implemented
/// without warnings.
@available(SwiftStdlib 5.1, *)
final class OldExecutorOldStdlib: SerialExecutor {
  func enqueue(_ job: UnownedJob) {}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Implementing both enqueue methods is legal, but somewhat useless --
/// we call into the "old one"; so the Owned version is not used in such impl.
///
/// That's why we do log the deprecation warning, people should use the move-only version.
@available(SwiftStdlib 5.9, *)
final class BothExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {} // expected-warning{{'Executor.enqueue(UnownedJob)' is deprecated as a protocol requirement; conform type 'BothExecutor' to 'Executor' by implementing 'func enqueue(ExecutorJob)' instead}}

  func enqueue(_ job: __owned ExecutorJob) {}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Meanwhile, we warn on the ExecutorJob witness if the type has a broader
/// availability, since in this case the UnownedJob version needs to exist.
@available(SwiftStdlib 5.1, *)
final class BothExecutorOld: SerialExecutor {
  func enqueue(_ job: UnownedJob) {}

  @available(SwiftStdlib 5.9, *)
  func enqueue(_ job: __owned ExecutorJob) {} // expected-warning{{'Executor.enqueue(ExecutorJob)' will never be used, due to the presence of 'enqueue(UnownedJob)'}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// For now we must keep all 3 implementation kinds and warn about deprecated ones
@available(SwiftStdlib 5.9, *)
final class TripleExecutor: SerialExecutor {
  func enqueue(_ job: UnownedJob) {} // expected-warning{{'Executor.enqueue(UnownedJob)' is deprecated as a protocol requirement; conform type 'TripleExecutor' to 'Executor' by implementing 'func enqueue(ExecutorJob)' instead}}

  // expected-warning@+2{{'Job' is deprecated: renamed to 'ExecutorJob'}}
  // expected-note@+1{{use 'ExecutorJob' instead}}
  func enqueue(_ job: __owned Job) {} // expected-warning{{'Executor.enqueue(Job)' is deprecated as a protocol requirement; conform type 'TripleExecutor' to 'Executor' by implementing 'func enqueue(ExecutorJob)' instead}}

  func enqueue(_ job: consuming ExecutorJob) {}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Even though we do provide default impls for both enqueue requirements,
/// we manually detect and emit an error if neither of them is implemented.
///
/// We do so because we implement them recursively, so one of them must be implemented basically.
@available(SwiftStdlib 5.9, *)
final class NoneExecutor: SerialExecutor { // expected-error{{type 'NoneExecutor' does not conform to protocol 'Executor'}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Job still is deprecated
@available(SwiftStdlib 5.9, *)
final class StillDeprecated: SerialExecutor {
  // expected-warning@+2{{'Job' is deprecated: renamed to 'ExecutorJob'}}
  // expected-note@+1{{use 'ExecutorJob' instead}}
  func enqueue(_ job: __owned Job) {} // expected-warning{{'Executor.enqueue(Job)' is deprecated as a protocol requirement; conform type 'StillDeprecated' to 'Executor' by implementing 'func enqueue(ExecutorJob)' instead}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

/// Just implementing the new signature causes no warnings, good.
@available(SwiftStdlib 5.9, *)
final class NewExecutor: SerialExecutor {
  func enqueue(_ job: consuming ExecutorJob) {} // no warnings

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

// Good impl, but missing the ownership keyword
@available(SwiftStdlib 5.9, *)
final class MissingOwnership: SerialExecutor {
  func enqueue(_ job: ExecutorJob) {} // expected-error{{noncopyable parameter must specify its ownership}}
  // expected-note@-1{{add 'borrowing' for an immutable reference}}
  // expected-note@-2{{add 'inout' for a mutable reference}}
  // expected-note@-3{{add 'consuming' to take the value from the caller}}

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}
