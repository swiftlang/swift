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
  nonisolated func checkNonisolatedFunc() async {
    _warnUnexpectedIsolation(expected: self, message: "Whoops, nonisolated but expected self")
  }

  func checkIsolatedFunc() async {
    _warnUnexpectedIsolation(expected: self, message: "Whoops!")
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

  nonisolated func checkNonisolatedFunc() async {
    _warnUnexpectedIsolation(expected: self, message: "Whoops, nonisolated but expected self")
  }

  func checkIsolatedFunc() async {
    _warnUnexpectedIsolation(expected: self, message: "Whoops!")
  }
}


@MainActor
func checkNonisolatedMainActorFunc(expected: any Actor) async {
  _warnUnexpectedIsolation(expected: expected, message: "Whoops, MainActor but expected actor")
}

@main struct Main {
  static func main() async {
    let defaultActor = SomeDefaultActor()
    await defaultActor.checkNonisolatedFunc()
    await checkNonisolatedMainActorFunc(expected: defaultActor)

    let queueActor = ActorOnNaiveQueueExecutor()
    await queueActor.checkNonisolatedFunc()
    // CHECK: unexpected

    await checkNonisolatedMainActorFunc(expected: queueActor)
    // CHECK: NOPE

    print("isolated func: ") // CHECK: isolated func:
    await queueActor.checkIsolatedFunc()

    print("Done") // CHECK: Done
  }
}
