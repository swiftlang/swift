// RUN: %empty-directory(%t)
// RUN: %target-build-swift %import-libdispatch -Xfrontend -disable-availability-checking -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

@available(SwiftStdlib 6.2, *)
final class IsIsolatingExecutor: SerialExecutor {
  init() {}

  func enqueue(_ job: consuming ExecutorJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  func checkIsolated() {
    print("called: checkIsolated")
  }

  func isIsolatingCurrentContext() -> Bool? {
    print("called: isIsolatingCurrentContext")
    return true
  }
}

@available(SwiftStdlib 6.2, *)
final class JustCheckIsolatedExecutor: SerialExecutor {
  init() {}

  func enqueue(_ job: consuming ExecutorJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }

  func checkIsolated() {
    print("called: checkIsolated")
  }
}

@available(SwiftStdlib 6.2, *)
actor ActorOnIsCheckImplementingExecutor<Ex: SerialExecutor> {
  let executor: Ex

  init(on executor: Ex) {
    self.executor = executor
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
    let hasIsIsolatingCurrentContextExecutor = IsIsolatingExecutor()
    let justCheckIsolatedExecutor = JustCheckIsolatedExecutor()

    print("do checkIsolated with executor which does NOT implement isIsolatingCurrentContext")
    let checkIsolatedActor = ActorOnIsCheckImplementingExecutor(on: justCheckIsolatedExecutor)
    await checkIsolatedActor.checkPreconditionIsolated()
    // CHECK: Before preconditionIsolated
    // CHECK-NOT: called: isIsolatingCurrentContext
    // CHECK: called: checkIsolated
    // CHECK-NOT: called: isIsolatingCurrentContext
    // CHECK: After preconditionIsolated

    // CHECK: Before assumeIsolated
    // CHECK-NOT: called: isIsolatingCurrentContext
    // CHECK: called: checkIsolated
    // CHECK-NOT: called: isIsolatingCurrentContext
    // CHECK: After assumeIsolated
  }
}
