// RUN: %target-run-simple-swift( -O -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s --dump-input=always
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

// CHECK-NEXT: foo - withTaskExecutorPreference - withTaskCancellationHandler
// CHECK-NEXT: nonisolated(nonsending) someFunc() async
// CHECK-NEXT: foo - withTaskExecutorPreference - withTaskCancellationHandler (after someFunc)
// CHECK-NEXT: [executor][task-executor] Enqueue (2)
// CHECK-NEXT: foo - withTaskExecutorPreference - withTaskCancellationHandler done
// CHECK-NEXT: [executor][task-executor] Enqueue (3)
// CHECK-NEXT: foo - withTaskExecutorPreference done

// CHECK: == Make: actor Foo
// CHECK-NEXT: ---------------------------------------

// Hop onto the actor executor:
// CHECK-NEXT: [executor][actor-executor] Enqueue (1)
// CHECK-NEXT: actor.foo

// Crucially, there must not be a hop before entering the operation:
// CHECK-NEXT: actor.foo - withTaskCancellationHandler
// CHECK-NEXT: nonisolated(nonsending) someFunc() async
// CHECK-NEXT: actor.foo - withTaskCancellationHandler (after someFunc)
// CHECK-NEXT: [executor][actor-executor] Enqueue (2)
// CHECK-NEXT: actor.foo - withTaskCancellationHandler done

// No more enqueues are expected afterwards
// CHECK-NOT: [executor]

nonisolated(nonsending) func someFunc() async throws {
  print("nonisolated(nonsending) someFunc() async")
}

@available(SwiftStdlib 6.0, *)
@concurrent
func foo() async {
  await withTaskExecutorPreference(AssertExactEnqueueCountExecutor(maxEnqueues: 3, name: "task-executor")) {
    print("foo - withTaskExecutorPreference")

    await withTaskCancellationHandler {
      print("foo - withTaskExecutorPreference - withTaskCancellationHandler")
      try? await someFunc()
      print("foo - withTaskExecutorPreference - withTaskCancellationHandler (after someFunc)")
    } onCancel: {
      fatalError("Should not be cancelled")
    }
    print("foo - withTaskExecutorPreference - withTaskCancellationHandler done")
  }
  print("foo - withTaskExecutorPreference done")

  print("== Make: actor Foo")
  print("---------------------------------------")
  await Foo().foo()
}

@available(SwiftStdlib 6.0, *)
actor Foo {
  let exec = AssertExactEnqueueCountExecutor(maxEnqueues: 3, name: "actor-executor")
        
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.exec.asUnownedSerialExecutor()
  }
          
  func foo() async {
    print("actor.foo")

    await withTaskCancellationHandler {
      print("actor.foo - withTaskCancellationHandler")
      try? await someFunc()
      print("actor.foo - withTaskCancellationHandler (after someFunc)")
    } onCancel: {
      fatalError("Should not be cancelled")
    }
    print("actor.foo - withTaskCancellationHandler done")
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
    job.runSynchronously(
      isolatedTo: self.asUnownedSerialExecutor(),
      taskExecutor: self.asUnownedTaskExecutor())
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    UnownedSerialExecutor(ordinary: self)
  }
}

print("done") // CHECK: done
