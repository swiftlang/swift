// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s --dump-input=always
// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding
// REQUIRES: libdispatch
// REQUIRES: synchronization

import Synchronization


if #available(SwiftStdlib 6.0, *) {
  print("=== foo() async")
  print("---------------------------------------")
  await foo()
}

// CHECK: === foo() async
// CHECK-NEXT: ---------------------------------------
// We hop to the task executor:
// CHECK-NEXT: [executor][task-executor] Enqueue (1)

// CHECK-NEXT: foo - withTaskExecutorPreference

// CHECK: foo - withTaskExecutorPreference - withCheckedContinuation
// CHECK-NEXT: foo - withTaskExecutorPreference - withCheckedContinuation done

// CHECK: foo - withTaskExecutorPreference - withUnsafeContinuation
// CHECK-NEXT: foo - withTaskExecutorPreference - withUnsafeContinuation done

// CHECK: foo - withTaskExecutorPreference - withCheckedThrowingContinuation
// CHECK-NEXT: foo - withTaskExecutorPreference - withCheckedThrowingContinuation done

// CHECK: foo - withTaskExecutorPreference - withUnsafeThrowingContinuation
// CHECK-NEXT: foo - withTaskExecutorPreference - withUnsafeThrowingContinuation done

// By checking that this is the second enqueue here,
// we check that there was no stray enqueues between with... invocations:
// CHECK-NEXT: [executor][task-executor] Enqueue (2)

// CHECK-NEXT: foo - withTaskExecutorPreference done

// CHECK-NEXT: == Make: actor Foo
// CHECK-NEXT: ---------------------------------------
// CHECK-NEXT: [executor][actor-executor] Enqueue (1)
// CHECK-NEXT: actor.foo

// CHECK: actor.foo - withCheckedContinuation
// CHECK-NEXT: actor.foo - withCheckedContinuation done

// CHECK: actor.foo - withUnsafeContinuation
// CHECK-NEXT: actor.foo - withUnsafeContinuation done

// CHECK: actor.foo - withCheckedThrowingContinuation
// CHECK-NEXT: actor.foo - withCheckedThrowingContinuation done

// CHECK: actor.foo - withUnsafeThrowingContinuation
// CHECK-NEXT: actor.foo - withUnsafeThrowingContinuation done
// CHECK-NEXT: actor.foo done

// No more enqueues are expected afterwards
// CHECK-NOT: [executor]

@available(SwiftStdlib 6.0, *)
@concurrent
func foo() async {
  await withTaskExecutorPreference(AssertExactEnqueueCountExecutor(maxEnqueues: 2, name: "task-executor")) {
    print("foo - withTaskExecutorPreference")
    await withCheckedContinuation { cont in
      print("foo - withTaskExecutorPreference - withCheckedContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withCheckedContinuation done")

    await withUnsafeContinuation { cont in
      print("foo - withTaskExecutorPreference - withUnsafeContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withUnsafeContinuation done")

    try! await withCheckedThrowingContinuation { cont in
      print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation done")

    try! await withUnsafeThrowingContinuation { cont in
      print("foo - withTaskExecutorPreference - withUnsafeThrowingContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withUnsafeThrowingContinuation done")
  }
  print("foo - withTaskExecutorPreference done")

  print("== Make: actor Foo")
  print("---------------------------------------")
  await Foo().foo()
}

@available(SwiftStdlib 6.0, *)
actor Foo {
  let exec = AssertExactEnqueueCountExecutor(maxEnqueues: 2, name: "actor-executor")

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.exec.asUnownedSerialExecutor()
  }

  func foo() async {
    print("actor.foo")

    await withCheckedContinuation { cont in
      print("actor.foo - withCheckedContinuation")
      cont.resume()
    }
    print("actor.foo - withCheckedContinuation done")

    await withUnsafeContinuation { cont in
      print("actor.foo - withUnsafeContinuation")
      cont.resume()
    }
    print("actor.foo - withUnsafeContinuation done")

    try! await withCheckedThrowingContinuation { cont in
      print("actor.foo - withCheckedThrowingContinuation")
      cont.resume()
    }
    print("actor.foo - withCheckedThrowingContinuation done")

    try! await withUnsafeThrowingContinuation { cont in
      print("actor.foo - withUnsafeThrowingContinuation")
      cont.resume()
    }
    print("actor.foo - withUnsafeThrowingContinuation done")

    print("actor.foo done")
  }
}

@available(SwiftStdlib 6.0, *)
final class AssertExactEnqueueCountExecutor: TaskExecutor, SerialExecutor {
  let maxEnqueues: Int
  let enqueueCount: Atomic<Int>

  let name: String

  init(maxEnqueues: Int, name: String) {
    self.maxEnqueues = maxEnqueues
    self.enqueueCount = .init(0)
    self.name = name
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let newEnqueueValue = self.enqueueCount.add(1, ordering: .relaxed).newValue
    if newEnqueueValue > self.maxEnqueues {
      fatalError("Got unexpected enqueue (\(newEnqueueValue)), in: \(self.name)")
    }
    print("[executor][\(self.name)] Enqueue (\(newEnqueueValue))")
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

print("done") // CHECK: done
