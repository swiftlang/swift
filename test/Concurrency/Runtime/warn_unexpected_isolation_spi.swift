// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library -plugin-path %swift-plugin-dir %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/a.out 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import StdlibUnittest
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
    _warnUnexpectedIsolation(expected: self, message: "Whoops, nonisolated but expected self", line: line)
  }

  func checkIsolatedFunc(line: Int) async {
    _warnUnexpectedIsolation(expected: self, message: "Whoops!", line: line)
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

  nonisolated func checkNonisolatedFunc(line: Int) async {
    _warnUnexpectedIsolation(expected: self, message: "Whoops, nonisolated but expected self", line: line)
  }

  func checkIsolatedFunc(line: Int) async {
    _warnUnexpectedIsolation(expected: self, message: "Whoops!")
  }
}


@MainActor
func checkNonisolatedMainActorFunc(expected: any Actor, line: Int) async {
  _warnUnexpectedIsolation(expected: expected, message: "Whoops, MainActor but expected actor")
}

@main struct Main {
  static func main() async {
    let defaultActor = SomeDefaultActor()

    await defaultActor.checkNonisolatedFunc(line: #line)
    // CHECK: Unexpected actor isolation, expected [[DEFAULT_ACTOR:0x.*]]
    // CHECK-SAME: (default actor a.SomeDefaultActor) but was nonisolated 0x0 (GenericExecutor)
    // CHECK-SAME: in checkNonisolatedFunc(line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-1]]!
    // CHECK-SAME: Whoops, nonisolated but expected self

    await checkNonisolatedMainActorFunc(expected: defaultActor, line: #line)
    // CHECK: Unexpected actor isolation, expected [[DEFAULT_ACTOR]] (default actor a.SomeDefaultActor)
    // CHECK-SAME: but was isolated to [[MAIN_ACTOR:0x.*]] (MainActorExecutor),
    // CHECK-SAME: in checkNonisolatedMainActorFunc(expected:line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-1]]!
    // CHECK-SAME: Whoops, MainActor but expected actor

    await defaultActor.checkIsolatedFunc(line: #line)
    // OK

    let queueActor = ActorOnNaiveQueueExecutor()
    await queueActor.checkNonisolatedFunc(line: #line)
    // CHECK: Unexpected actor isolation, expected [[QUEUE_ACTOR:0x.*]]
    // CHECK-SAME: but was nonisolated 0x0 (GenericExecutor)
    // CHECK-SAME: in checkNonisolatedFunc(line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:58!
    // CHECK-SAME: Whoops, nonisolated but expected self

    await checkNonisolatedMainActorFunc(expected: queueActor, line: #line)
    // CHECK: Unexpected actor isolation, expected [[QUEUE_ACTOR]]
    // CHECK-SAME: but was isolated to [[MAIN_ACTOR]] (MainActorExecutor),
    // CHECK-SAME: in checkNonisolatedMainActorFunc(expected:line:)
    // CHECK-SAME: at a/warn_unexpected_isolation_spi.swift:[[@LINE-1]]!
    // CHECK-SAME: Whoops, MainActor but expected actor

    await queueActor.checkIsolatedFunc(line: #line)
    // OK

    print("Done")
    // CHECK: Done
  }
}
