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

@_spi(CustomDefaultExecutors) import _Concurrency

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
final class UnknownIfIsIsolatingExecutor: SerialExecutor {
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
    print("called: isIsolatingCurrentContext; return nil")
    return nil
  }
}

@available(SwiftStdlib 6.2, *)
final class NoChecksImplementedExecutor: SerialExecutor {
  init() {}

  func enqueue(_ job: consuming ExecutorJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
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

  func checkIsIsolatingCurrentContext() async -> Bool? {
    executor.isIsolatingCurrentContext()
  }
}

@main struct Main {
  static func main() async {
    let hasIsIsolatingCurrentContextExecutor = IsIsolatingExecutor()
    let hasIsCheckActor = ActorOnIsCheckImplementingExecutor(on: hasIsIsolatingCurrentContextExecutor)
    let unknownIsIsolatingActor = ActorOnIsCheckImplementingExecutor(on: UnknownIfIsIsolatingExecutor())

    let anyActor: any Actor = hasIsCheckActor

    anyActor.withSerialExecutor { se in
      let outside = se.isIsolatingCurrentContext()
      assert(outside == true) // This is just a mock executor impl that always returns "true" (it is lying)
      // CHECK: called: isIsolatingCurrentContext
    }

    let inside = await hasIsCheckActor.checkIsIsolatingCurrentContext()
    assert(inside == true)
    // CHECK: called: isIsolatingCurrentContext

    _ = unknownIsIsolatingActor.preconditionIsolated()
    // CHECK: called: isIsolatingCurrentContext; return nil
    // CHECK: called: checkIsolated
  }
}
