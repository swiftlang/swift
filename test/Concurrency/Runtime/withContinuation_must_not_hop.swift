// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s --dump-input=always
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
  let expectedEnqueueCount: Int
  let enqueueCount: Atomic<Int>

  let name: String
  
  init(expectedEnqueueCount: Int, name: String) {
    self.expectedEnqueueCount = expectedEnqueueCount
    self.enqueueCount = .init(0)
    self.name = name
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    let newEnqueueValue = self.enqueueCount.add(1, ordering: .relaxed).newValue
    if newEnqueueValue > self.expectedEnqueueCount {
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
// CHECK: ---------------------------------------
// We hop to the task executor:
// CHECK: Executor(task-executor): enqueue (1)

// CHECK: foo - withTaskExecutorPreference

// CHECK: foo - withTaskExecutorPreference - withCheckedContinuation
// CHECK: foo - withTaskExecutorPreference - withCheckedContinuation done

// CHECK: foo - withTaskExecutorPreference - withUnsafeContinuation
// CHECK: foo - withTaskExecutorPreference - withUnsafeContinuation done

// CHECK: foo - withTaskExecutorPreference - withCheckedThrowingContinuation
// CHECK: foo - withTaskExecutorPreference - withCheckedThrowingContinuation done

// CHECK: foo - withTaskExecutorPreference - withUnsafeThrowingContinuation
// CHECK: foo - withTaskExecutorPreference - withUnsafeThrowingContinuation done

// By checking that this is the second enqueue here,
// we check that there was no stray enqueues between with... invocations:
// CHECK: Executor(task-executor): enqueue (2)

// CHECK: foo - withTaskExecutorPreference done

// CHECK: == Make: actor Foo
// CHECK: ---------------------------------------
// CHECK: Executor(actor-executor): enqueue (1)
// CHECK: actor.foo

// CHECK: actor.foo - withCheckedContinuation
// CHECK: actor.foo - withCheckedContinuation done

// CHECK: actor.foo - withUnsafeContinuation
// CHECK: actor.foo - withUnsafeContinuation done

// CHECK: actor.foo - withCheckedThrowingContinuation
// CHECK: actor.foo - withCheckedThrowingContinuation done

// CHECK: actor.foo - withUnsafeThrowingContinuation
// CHECK: actor.foo - withUnsafeThrowingContinuation done
// CHECK: actor.foo done

// No more enqueues are expected afterwards
// CHECK-NOT: Executor

@available(SwiftStdlib 6.0, *)
@concurrent
func foo() async {
  await withTaskExecutorPreference(MyExec(name: "task-executor")) {
    print("foo - withTaskExecutorPreference")
    await withCheckedContinuation { cont in
      print("foo - withTaskExecutorPreference - withCheckedContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withCheckedContinuation done\n")

    await withUnsafeContinuation { cont in
      print("foo - withTaskExecutorPreference - withUnsafeContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withUnsafeContinuation done\n")

    try! await withCheckedThrowingContinuation { cont in
      print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withCheckedThrowingContinuation done\n")

    try! await withUnsafeThrowingContinuation { cont in
      print("foo - withTaskExecutorPreference - withUnsafeThrowingContinuation")
      cont.resume()
    }
    print("foo - withTaskExecutorPreference - withUnsafeThrowingContinuation done\n")
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

    await withCheckedContinuation { cont in
      print("actor.foo - withCheckedContinuation")
      cont.resume()
    }
    print("actor.foo - withCheckedContinuation done\n")

    await withUnsafeContinuation { cont in
      print("actor.foo - withUnsafeContinuation")
      cont.resume()
    }
    print("actor.foo - withUnsafeContinuation done\n")
    
    try! await withCheckedThrowingContinuation { cont in
      print("actor.foo - withCheckedThrowingContinuation")
      cont.resume()
    }
    print("actor.foo - withCheckedThrowingContinuation done\n")
    
    try! await withUnsafeThrowingContinuation { cont in
      print("actor.foo - withUnsafeThrowingContinuation")
      cont.resume()
    }
    print("actor.foo - withUnsafeThrowingContinuation done\n")

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
