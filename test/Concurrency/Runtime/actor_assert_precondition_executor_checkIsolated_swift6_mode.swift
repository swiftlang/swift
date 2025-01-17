// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

final class NaiveQueueExecutor: SerialExecutor {
  init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  func checkIsolated() {
    print("checkIsolated: pretend it is ok!")
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

  nonisolated func checkPreconditionIsolated() async {
    print("Before preconditionIsolated")
    self.preconditionIsolated()
    print("After preconditionIsolated")

    print("Before assumeIsolated")
    self.assumeIsolated { iso in
      print("Inside assumeIsolated")
    }
    print("After assumeIsolated")
  }
}

@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 6.0, *) {
      let actor = ActorOnNaiveQueueExecutor()
      await actor.checkPreconditionIsolated()
      // CHECK: Before preconditionIsolated
      // CHECK-NEXT: checkIsolated: pretend it is ok!
      // CHECK-NEXT: After preconditionIsolated

      // CHECK-NEXT: Before assumeIsolated
      // CHECK-NEXT: checkIsolated: pretend it is ok!
      // CHECK-NEXT: Inside assumeIsolated
      // CHECK-NEXT: After assumeIsolated
    }
  }
}
