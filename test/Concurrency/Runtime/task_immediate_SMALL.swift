// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s %import-libdispatch -swift-version 6 -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import _Concurrency
import Dispatch

enum CompareHow {
  case equal
  case notEqual

  func check(_ wasEqual: Bool) -> Bool {
    switch self {
    case .equal: wasEqual
    case .notEqual: !wasEqual
    }
  }
}

#if canImport(Darwin)
import Darwin
typealias ThreadID = pthread_t
func getCurrentThreadID() -> ThreadID { pthread_self() }
func compareThreadIDs(_ a: ThreadID, _ how: CompareHow, _ b: ThreadID) -> Bool {
  how.check(pthread_equal(a, b) != 0)
}
#elseif canImport(Glibc)
import Glibc
typealias ThreadID = pthread_t
func getCurrentThreadID() -> ThreadID { pthread_self() }
func compareThreadIDs(_ a: ThreadID, _ how: CompareHow, _ b: ThreadID) -> Bool {
  how.check(pthread_equal(a, b) != 0)
}
#elseif os(Windows)
import WinSDK
typealias ThreadID = UInt32
func getCurrentThreadID() -> ThreadID { GetCurrentThreadId() }
func compareThreadIDs(_ a: ThreadID, _ how: CompareHow, _ b: ThreadID) -> Bool {
  how.check(a == b)
}
#elseif os(WASI)
typealias ThreadID = UInt32
func getCurrentThreadID() -> ThreadID { 0 }
func compareThreadIDs(_ a: ThreadID, _ how: CompareHow, _ b: ThreadID) -> Bool {
  how.check(a == b)
}
#endif
extension ThreadID: @unchecked Sendable {}

final class NaiveQueueExecutor: SerialExecutor {
  let queue: DispatchQueue

  init(queue: DispatchQueue) {
    self.queue = queue
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let unowned = UnownedJob(job)
    print("NaiveQueueExecutor(\(self.queue.label)) enqueue [thread:\(getCurrentThreadID())]")
    queue.async {
      unowned.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

@globalActor
actor DifferentGlobalActor {
  static let queue = DispatchQueue(label: "DifferentGlobalActor-queue")
  let executor: NaiveQueueExecutor
  nonisolated let unownedExecutor: UnownedSerialExecutor

  init() {
    self.executor = NaiveQueueExecutor(queue: DifferentGlobalActor.queue)
    self.unownedExecutor = executor.asUnownedSerialExecutor()
  }

  static let shared: DifferentGlobalActor = DifferentGlobalActor()

  @DifferentGlobalActor
  static func test() {}
}

print("\n\n==== ------------------------------------------------------------------")
print("inherit actor context without closing over isolated reference")

actor A {
  static let queue = DispatchQueue(label: "DifferentGlobalActor-queue")
  let executor: NaiveQueueExecutor
  nonisolated let unownedExecutor: UnownedSerialExecutor

  init() {
    self.executor = NaiveQueueExecutor(queue: DifferentGlobalActor.queue)
    self.unownedExecutor = executor.asUnownedSerialExecutor()
  }

  func foo() async {
    let task = Task.immediate {
      // doesn't capture self (!)
      dispatchPrecondition(condition: .onQueue(DifferentGlobalActor.queue))
    }

    await task.value
  }
}

await Task {
  await A().foo()
  print("OK")
}.value
// CHECK: OK