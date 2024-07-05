// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library -plugin-path %swift-plugin-dir %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy SWIFT_LOG_ISOLATION_WARNING_MODE_OVERRIDE=stderr %target-run %t/a.out 2>&1 | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import Dispatch
@_spi(ConcurrencyDiagnostics) import _Concurrency

final class NaiveQueueExecutor: SerialExecutor {
  init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  func checkIsolated() {
    fatalError("Should not be invoked for warning-only SPI")
  }
}

actor SomeDefaultActor {
  nonisolated func checkNonisolatedFunc(line: Int) async {
    _warnOnUnexpectedExecutor(expected: self, message: "Whoops, nonisolated but expected self", line: line)
  }

  func checkIsolatedFunc(line: Int) async {
    _warnOnUnexpectedExecutor(expected: self, message: "Whoops!", line: line)
  }
}

actor ActorOnNaiveQueueExecutor {
  let executor: NaiveQueueExecutor

  init() {
    self.executor = NaiveQueueExecutor()
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }

  nonisolated func checkNonisolatedFuncOnQueueActor(line: Int) async {
    _warnOnUnexpectedExecutor(expected: self, message: "Whoops, nonisolated but expected self", line: line)
  }

  func checkIsolatedFunc(line: Int) async {
    _warnOnUnexpectedExecutor(expected: self, message: "Whoops!", line: line)
  }
}

@MainActor
func checkMainActorFunc(expected: any Actor, line: Int) async {
  _warnOnUnexpectedExecutor(expected: expected, message: "Whoops, MainActor but expected actor", line: line)
}

func checkOtherActorFuncExpectedMainActor(other: isolated (any Actor), line: Int) async {
  _warnOnUnexpectedExecutor(expected: MainActor.shared, message: "Whoops, MainActor but expected actor", line: line)
}

@main struct Main {
  static func main() async {
    let defaultActor = SomeDefaultActor()
    let queueWrappedActor = ActorOnNaiveQueueExecutor()

    // NOTE: Watch out for matching nil/0x0, as it seems 0x0 gets formatted as nil on Linux, so match both.

    await defaultActor.checkNonisolatedFunc(line: #line)
    // CHECK: Unexpected actor isolation, expected [[DEFAULT_ACTOR:0x.*]] (default actor a.SomeDefaultActor)
    // CHECK-SAME: but was nonisolated [[NONISOLATED:(0x.*)|.nil.]] (GenericExecutor),
    // CHECK-SAME: in checkNonisolatedFunc(line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-4]]:[[COLUMN:.*]]!
    // CHECK-SAME: Whoops, nonisolated but expected self


    await checkMainActorFunc(expected: defaultActor, line: #line)
    // CHECK: Unexpected actor isolation, expected [[DEFAULT_ACTOR:0x.*]] (default actor a.SomeDefaultActor)
    // CHECK-SAME: but was isolated to [[MAIN_ACTOR:0x.*]] (MainActorExecutor),
    // CHECK-SAME: in checkMainActorFunc(expected:line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-4]]:[[COLUMN:.*]]!
    // CHECK-SAME: Whoops, MainActor but expected actor

    await defaultActor.checkIsolatedFunc(line: #line)
    // OK

    await queueWrappedActor.checkNonisolatedFuncOnQueueActor(line: #line)
    // CHECK: Unexpected actor isolation, expected [[QUEUE_ACTOR:0x.*]] (custom SerialExecutor)
    // CHECK-SAME: but was nonisolated [[GENERIC_EXEC:.nil.|0x0]] (GenericExecutor)
    // CHECK-SAME: in checkNonisolatedFuncOnQueueActor(line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-4]]:[[COLUMN:.*]]!
    // CHECK-SAME: Whoops, nonisolated but expected self

    await checkMainActorFunc(expected: queueWrappedActor, line: #line)
    // CHECK: Unexpected actor isolation, expected [[QUEUE_ACTOR]] (custom SerialExecutor)
    // CHECK-SAME: but was isolated to [[MAIN_ACTOR]] (MainActorExecutor),
    // CHECK-SAME: in checkMainActorFunc(expected:line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-4]]:[[COLUMN:.*]]!
    // CHECK-SAME: Whoops, MainActor but expected actor

    await queueWrappedActor.checkIsolatedFunc(line: #line)
    // OK

    // Expecting main actor, but running on something else
    await checkOtherActorFuncExpectedMainActor(other: defaultActor, line: #line)
    // CHECK: Unexpected actor isolation, expected [[MAIN_ACTOR]] (MainActorExecutor)
    // CHECK-SAME: but was isolated to [[DEFAULT_ACTOR:0x.*]] (default actor a.SomeDefaultActor),
    // CHECK-SAME: in checkOtherActorFuncExpectedMainActor(other:line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-4]]:[[COLUMN:.*]]!
    // CHECK-SAME: Whoops, MainActor but expected actor

    await checkOtherActorFuncExpectedMainActor(other: queueWrappedActor, line: #line)
    // CHECK: Unexpected actor isolation, expected [[MAIN_ACTOR]] (MainActorExecutor)
    // CHECK-SAME: but was isolated to [[QUEUE_ACTOR:0x.*]] (custom SerialExecutor),
    // CHECK-SAME: in checkOtherActorFuncExpectedMainActor(other:line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-4]]:[[COLUMN:.*]]!
    // CHECK-SAME: Whoops, MainActor but expected actor

    print("Done")
    // CHECK: Done
  }
}
