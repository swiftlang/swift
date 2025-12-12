// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s --dump-input=always
// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding
// REQUIRES: libdispatch
// REQUIRES: synchronization

import Synchronization
import Dispatch


if #available(SwiftStdlib 6.0, *) {
  print("=== foo() async")
  print("---------------------------------------")
  await foo()
}

// CHECK: === foo() async
// CHECK-NEXT: ---------------------------------------

// We hop to the task executor:
// CHECK: foo - withTaskExecutorPreference before
// CHECK: [executor][task-executor] Enqueue (1)
// CHECK: foo - withTaskExecutorPreference

// CHECK: foo - withTaskExecutorPreference - withCheckedContinuation before
// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: foo - withTaskExecutorPreference - withCheckedContinuation
// CHECK: foo - withTaskExecutorPreference - withCheckedContinuation done

// CHECK: foo - withTaskExecutorPreference - withUnsafeContinuation before
// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: foo - withTaskExecutorPreference - withUnsafeContinuation
// CHECK: foo - withTaskExecutorPreference - withUnsafeContinuation done

// CHECK: foo - withTaskExecutorPreference - withCheckedThrowingContinuation before
// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: foo - withTaskExecutorPreference - withCheckedThrowingContinuation
// CHECK: foo - withTaskExecutorPreference - withCheckedThrowingContinuation done

// CHECK: foo - withTaskExecutorPreference - withUnsafeThrowingContinuation before
// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: foo - withTaskExecutorPreference - withUnsafeThrowingContinuation done

// By checking that this is the second enqueue here,
// we check that there was no stray enqueues between with... invocations:
// NOT: [executor][task-executor] Enqueue (2)

// CHECK: foo - withTaskExecutorPreference done

// CHECK-NEXT: == Make: actor Foo
// CHECK-NEXT: ---------------------------------------
// CHECK: [executor][actor-executor] Enqueue (1)
// CHECK: actor.foo

// CHECK: actor.foo - withCheckedContinuation before
// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: actor.foo - withCheckedContinuation
// CHECK: actor.foo - withCheckedContinuation done

// CHECK: actor.foo - withUnsafeContinuation before
// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: actor.foo - withUnsafeContinuation
// CHECK: actor.foo - withUnsafeContinuation done

// CHECK: actor.foo - withCheckedThrowingContinuation before
// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: actor.foo - withCheckedThrowingContinuation
// CHECK: actor.foo - withCheckedThrowingContinuation done

// CHECK: actor.foo - withUnsafeThrowingContinuation before
// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: actor.foo - withUnsafeThrowingContinuation
// CHECK: actor.foo - withUnsafeThrowingContinuation done

// CHECK-NOT: [executor][task-executor] Enqueue
// CHECK: actor.foo done

// No more enqueues are expected afterwards
// CHECK-NOT: [executor]

@available(SwiftStdlib 6.0, *)
@concurrent
func foo() async {
  print("foo - withTaskExecutorPreference before")
  await withTaskExecutorPreference(AssertExactEnqueueCountExecutor(name: "task-executor")) {
    print("foo - withTaskExecutorPreference")

    // --- 

    print("foo - withTaskExecutorPreference - withCheckedContinuation before")
    await withCheckedContinuation { cont in
      print("foo - withTaskExecutorPreference - withCheckedContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withCheckedContinuation done")

    // --- 

    print("foo - withTaskExecutorPreference - withUnsafeContinuation before")
    await withUnsafeContinuation { cont in
      print("foo - withTaskExecutorPreference - withUnsafeContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withUnsafeContinuation done")

    // --- 

    print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation before")
    try! await withCheckedThrowingContinuation { cont in
      print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation done")

    // --- 

    print("foo - withTaskExecutorPreference - withUnsafeThrowingContinuation before")
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
  let exec = AssertExactEnqueueCountExecutor(name: "actor-executor")

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.exec.asUnownedSerialExecutor()
  }

  func foo() async {
    print("actor.foo")

    print("actor.foo - withCheckedContinuation before")
    await withCheckedContinuation { cont in
      print("actor.foo - withCheckedContinuation")
      cont.resume()
    }
    print("actor.foo - withCheckedContinuation done")

    // --- 

    print("actor.foo - withUnsafeContinuation before")
    await withUnsafeContinuation { cont in
      print("actor.foo - withUnsafeContinuation")
      cont.resume()
    }
    print("actor.foo - withUnsafeContinuation done")
    
    // --- 

    print("actor.foo - withCheckedThrowingContinuation before")
    try! await withCheckedThrowingContinuation { cont in
      print("actor.foo - withCheckedThrowingContinuation")
      cont.resume()
    }
    print("actor.foo - withCheckedThrowingContinuation done")

    // --- 

    print("actor.foo - withUnsafeThrowingContinuation before")
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
  let enqueueCount: Atomic<Int>
  let queue: DispatchSerialQueue
  let name: String

  init(name: String) {
    self.enqueueCount = .init(0)
    self.queue = DispatchSerialQueue(label: name)
    self.name = name
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    queue.async {
      let newEnqueueValue = self.enqueueCount.add(1, ordering: .relaxed).newValue
      print("[executor][\(self.name)] Enqueue (\(newEnqueueValue))")
      job.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

print("done") // CHECK: done