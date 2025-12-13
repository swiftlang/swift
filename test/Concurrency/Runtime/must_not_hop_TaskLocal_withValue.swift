// RUN: %target-run-simple-swift( -O -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s --dump-input=always
// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding
// REQUIRES: libdispatch
// REQUIRES: synchronization

import Synchronization

@available(SwiftStdlib 6.0, *)
final class AssertExactEnqueueCountExecutor: SerialExecutor {
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


if #available(SwiftStdlib 6.0, *) {
  print("=== foo() async")
  print("---------------------------------------")
  await foo()
}

// CHECK: === foo() async
// CHECK-NEXT: ---------------------------------------
// We hop to the task executor:
// CHECK-NEXT: Executor(task-executor): enqueue (1)

// CHECK-NEXT: foo - withTaskExecutorPreference

// CHECK: foo - withTaskExecutorPreference - TL.withValue
// CHECK-NEXT: nonisolated(nonsending) someFunc() async
// TODO: could we eliminate this hop-back?
// CHECK-NEXT: Executor(task-executor): enqueue (2)
// CHECK-NEXT: foo - withTaskExecutorPreference - TL.withValue done

// CHECK: foo - withTaskExecutorPreference - TL.withValue throwing
// CHECK-NEXT: nonisolated(nonsending) someFunc() async
// CHECK-NEXT: Executor(task-executor): enqueue (3)
// CHECK-NEXT: foo - withTaskExecutorPreference - TL.withValue throwing done

// CHECK: Executor(task-executor): enqueue (4)
// CHECK: foo - withTaskExecutorPreference done

// CHECK: == Make: actor Foo
// CHECK-NEXT: ---------------------------------------
// CHECK-NEXT: [executor][actor-executor] Enqueue (1)
// CHECK-NEXT: actor.foo

// CHECK: actor.foo - withTaskExecutorPreference - TL.withValue
// CHECK-NEXT: nonisolated(nonsending) someFunc() async
// TODO: could we eliminate this hop-back?
// CHECK-NEXT: [executor][actor-executor] Enqueue (2)
// CHECK-NEXT: actor.foo - withTaskExecutorPreference - TL.withValue done

// CHECK: actor.foo - withTaskExecutorPreference - TL.withValue throwing
// CHECK-NEXT: nonisolated(nonsending) someFunc() async
// TODO: could we eliminate this hop-back?
// CHECK-NEXT: [executor][actor-executor] Enqueue (3)
// CHECK-NEXT: actor.foo - withTaskExecutorPreference - TL.withValue throwing done

// No more enqueues are expected afterwards
// CHECK-NOT: [executor]

@TaskLocal
let myTaskLocal: String = ""

nonisolated(nonsending) func someFunc() async throws {
  print("nonisolated(nonsending) someFunc() async")
}

@available(SwiftStdlib 6.0, *)
@concurrent
func foo() async {
  await withTaskExecutorPreference(MyExec(name: "task-executor")) {
    print("foo - withTaskExecutorPreference")

    await $myTaskLocal.withValue("value") {
      print("foo - withTaskExecutorPreference - TL.withValue")
      try? await someFunc()
    }
    print("foo - withTaskExecutorPreference - TL.withValue done\n")

    try! await $myTaskLocal.withValue("value") {
      print("foo - withTaskExecutorPreference - TL.withValue throwing")
      try await someFunc()
    }
    print("foo - withTaskExecutorPreference - TL.withValue throwing done\n")
  }
  print("foo - withTaskExecutorPreference done")

  print("== Make: actor Foo")
  print("---------------------------------------")
  await Foo().foo()
}

@available(SwiftStdlib 6.0, *)
actor Foo {
  let exec = MyExec(name: "actor-executor")
        
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.exec.asUnownedSerialExecutor()
  }
          
  func foo() async {
    print("actor.foo")

    await $myTaskLocal.withValue("value") {
      print("actor.foo - withTaskExecutorPreference - TL.withValue")
      try? await someFunc()
    }
    print("actor.foo - withTaskExecutorPreference - TL.withValue done\n")

    try! await $myTaskLocal.withValue("value") {
      print("actor.foo - withTaskExecutorPreference - TL.withValue throwing")
      try await someFunc()
    }
    print("actor.foo - withTaskExecutorPreference - TL.withValue throwing done\n")
    print("actor.foo done")
  }
}

@available(SwiftStdlib 6.0, *)
final class MyExec: TaskExecutor, SerialExecutor {
  let name: String
  let enqueueCount: Atomic<Int> = .init(0)

  init(name: String) {
    self.name = name
  }

  func enqueue(_ job: UnownedJob) {
    let count = self.enqueueCount.add(1, ordering: .relaxed)
    print("Executor(\(name)): enqueue (\(count.newValue))")

    job
    .runSynchronously(
      isolatedTo: self.asUnownedSerialExecutor(),
      taskExecutor: self.asUnownedTaskExecutor()
    )
  }
}

print("done") // CHECK: done
