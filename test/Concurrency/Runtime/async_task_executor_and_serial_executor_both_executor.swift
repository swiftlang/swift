// NOT: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library)

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %import-libdispatch %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=swift6 %target-run %import-libdispatch %t/a.out

// TODO: Need to find out how to combine %env- and %target-run and %import-libdispatch reliably.
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=freebsd

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: libdispatch
//
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch
import _Concurrency

import Dispatch

final class DispatchQueueBothExecutor: @unchecked Sendable, SerialExecutor, TaskExecutor {
  let queue: DispatchQueue

  init(queue: DispatchQueue) {
    self.queue = queue
  }

  func enqueue(_ job: UnownedJob) {
    queue.async {
      job.runSynchronously(
        isolatedTo: self.asUnownedSerialExecutor(),
        taskExecutor: self.asUnownedTaskExecutor())
    }
  }

  func checkIsolated() {
    dispatchPrecondition(condition: .onQueue(self.queue))
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    .init(ordinary: self)
  }
}

actor Kappa {
  func test() {
    self.preconditionIsolated()
  }
}

actor QueueActor {
  let executor: DispatchQueueBothExecutor

  init(executor: DispatchQueueBothExecutor) {
    self.executor = executor
  }

  func test() {
    self.preconditionIsolated()
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.executor.asUnownedSerialExecutor()
  }
}

@main struct Main {

  static func main() async {
    let executor = DispatchQueueBothExecutor(queue: DispatchQueue(label: "label"))
    let kappa = Kappa()

    await withTaskGroup(of: Void.self) { group in

      group.addTask(executorPreference: executor) {
        print("Task enqueued")

        await kappa.test()
        print("Actor called")
      }
    }

    let qa = QueueActor(executor: executor)
    await qa.test()
    print("Actor on queue executor called")

    print("DONE")
  }
}
