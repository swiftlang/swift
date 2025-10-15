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

// CHECK-NEXT: foo - withTaskExecutorPreference - withTaskGroup
// CHECK-NEXT: nonisolated(nonsending) someFunc() async
// CHECK-NEXT: foo - withTaskExecutorPreference - withTaskGroup (after someFunc)
// CHECK-NEXT: foo - withTaskExecutorPreference - withTaskGroup done

// CHECK-NEXT: foo - withTaskExecutorPreference - withThrowingTaskGroup
// CHECK-NEXT: nonisolated(nonsending) someFunc() async
// CHECK-NEXT: foo - withTaskExecutorPreference - withThrowingTaskGroup (after someFunc)
// TODO: can we reduce the number of enqueues here?
// CHECK-NEXT: [executor][task-executor] Enqueue (2)
// CHECK-NEXT: [executor][task-executor] Enqueue (3)
// CHECK-NEXT: [executor][task-executor] Enqueue (4)
// CHECK-NEXT: foo - withTaskExecutorPreference - withThrowingTaskGroup done

// CHECK-NEXT: [executor][task-executor] Enqueue (2)

// CHECK: foo - withTaskExecutorPreference done

// CHECK: == Make: actor Foo
// CHECK-NEXT: ---------------------------------------
// TODO: all the other group kinds...

// No more enqueues are expected afterwards
// CHECK-NOT: [executor]

nonisolated(nonsending) func someFunc() async throws {
  print("nonisolated(nonsending) someFunc() async")
}

@available(SwiftStdlib 6.0, *)
@concurrent
func foo() async {
  await withTaskExecutorPreference(AssertExactEnqueueCountExecutor(maxEnqueues: 8, name: "task-executor")) {
    print("foo - withTaskExecutorPreference")

    await withTaskGroup(of: Void.self) { group in 
      print("foo - withTaskExecutorPreference - withTaskGroup")
      try? await someFunc()
      print("foo - withTaskExecutorPreference - withTaskGroup (after someFunc)")
    }
    print("foo - withTaskExecutorPreference - withTaskGroup done")

    try! await withThrowingTaskGroup(of: Void.self) { group in
      print("foo - withTaskExecutorPreference - withThrowingTaskGroup")
      try await someFunc()
      print("foo - withTaskExecutorPreference - withThrowingTaskGroup (after someFunc)")
    }
    print("foo - withTaskExecutorPreference - withThrowingTaskGroup done")

    await withDiscardingTaskGroup { group in
      print("foo - withTaskExecutorPreference - withDiscardingTaskGroup")
      try? await someFunc()
      print("foo - withTaskExecutorPreference - withDiscardingTaskGroup (after someFunc)")
    }
    print("foo - withTaskExecutorPreference - withDiscardingTaskGroup done")

    try! await withThrowingDiscardingTaskGroup { group in
      print("foo - withTaskExecutorPreference - withThrowingDiscardingTaskGroup")
      try await someFunc()
      print("foo - withTaskExecutorPreference - withThrowingDiscardingTaskGroup (after someFunc)")
    }
    print("foo - withTaskExecutorPreference - withThrowingDiscardingTaskGroup done")
  }
  print("foo - withTaskExecutorPreference done")

  print("== Make: actor Foo")
  print("---------------------------------------")
  await Foo().foo()
}

@available(SwiftStdlib 6.0, *)
actor Foo {
  let exec = AssertExactEnqueueCountExecutor(maxEnqueues: 8, name: "actor-executor")
        
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.exec.asUnownedSerialExecutor()
  }
          
  func foo() async {
//    print("actor.foo")
//
//    await withTaskGroup(of: Void.self) { group in
//      print("actor.foo - withTaskExecutorPreference - withTaskGroup")
//      try? await someFunc()
//    }
//    print("actor.foo - withTaskExecutorPreference - withTaskGroup done\n")
//
//    try! await withThrowingTaskGroup(of: Void.self) { group in
//      print("actor.foo - withTaskExecutorPreference - withThrowingTaskGroup")
//      try await someFunc()
//    }
//    print("actor.foo - withTaskExecutorPreference - withThrowingTaskGroup done\n")
//    print("actor.foo done")
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
