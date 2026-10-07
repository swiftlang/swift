// RUN: %target-run-simple-swift( -target %target-swift-6.4-abi-triple %import-libdispatch -parse-as-library) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test

// UNSUPPORTED: freestanding

// UNSUPPORTED: back_deployment_runtime
// REQUIRES: concurrency_runtime
// REQUIRES: synchronization

// SE-0469: UnsafeCurrentTask access from ExecutorJob and UnownedJob

import Synchronization

let lastEnqueuedTask = Mutex<UnsafeCurrentTask?>(nil)

final class ExecutorJobExecutor: SerialExecutor {
  public func enqueue(_ job: consuming ExecutorJob) {
    let task = job.unsafeCurrentTask
    lastEnqueuedTask.withLock { $0 = task }
    print("ExecutorJob enqueue, task: \(task != nil), name: \(task?.name ?? "<no-name>")")
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }
}

final class UnownedJobExecutor: SerialExecutor {
  public func enqueue(_ job: UnownedJob) {
    let task = job.unsafeCurrentTask
    lastEnqueuedTask.withLock { $0 = task }
    print("UnownedJob enqueue, task: \(task != nil), name: \(task?.name ?? "<no-name>")")
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }
}

actor Custom {
  let executor: any SerialExecutor

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  init(executor: some SerialExecutor) {
    self.executor = executor
  }

  func check() {
    withUnsafeCurrentTask { current in
      let same = lastEnqueuedTask.withLock { $0 == current }
      print("same task: \(same)")
    }
  }
}

@main struct Main {
  static func main() async {
    for executor in [ExecutorJobExecutor() as any SerialExecutor, UnownedJobExecutor()] {
      let actor = Custom(executor: executor)

      await Task(name: "Caplin") {
        await actor.check()
      }.value

      await Task {
        await actor.check()
      }.value
    }

    // CHECK: ExecutorJob enqueue, task: true, name: Caplin
    // CHECK-NEXT: same task: true
    // CHECK: ExecutorJob enqueue, task: true, name: <no-name>
    // CHECK-NEXT: same task: true

    // CHECK: UnownedJob enqueue, task: true, name: Caplin
    // CHECK-NEXT: same task: true
    // CHECK: UnownedJob enqueue, task: true, name: <no-name>
    // CHECK-NEXT: same task: true
  }
}
