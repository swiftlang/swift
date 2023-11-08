// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

final class MyTaskExecutor: TaskExecutor, @unchecked Sendable {
  let queue: DispatchQueue

  init(queue: DispatchQueue) {
    self.queue = queue
  }

  func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    queue.async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }
}

nonisolated func nonisolatedAsyncMethod(expectedOn executor: MyTaskExecutor) async {
  dispatchPrecondition(condition: .onQueue(executor.queue))
}

@MainActor
func testNestingWithExecutor(_ firstExecutor: MyTaskExecutor,
                             _ secondExecutor: MyTaskExecutor) async {
  MainActor.preconditionIsolated()
  dispatchPrecondition(condition: .onQueue(DispatchQueue.main))

  await withTaskExecutor(firstExecutor) {
    // the block immediately hops to the expected executor
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    print("OK: withTaskExecutor body")
    await nonisolatedAsyncMethod(expectedOn: firstExecutor)
  }
  MainActor.preconditionIsolated()

  await withTaskExecutor(firstExecutor) {
    await withTaskExecutor(secondExecutor) {
      // the block immediately hops to the expected executor
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      print("OK: withTaskExecutor { withTaskExecutor { ... } }")
      await nonisolatedAsyncMethod(expectedOn: secondExecutor)
    }
  }
  MainActor.preconditionIsolated()

  await withTaskExecutor(firstExecutor) {
    await withTaskExecutor(secondExecutor) {
      await withTaskExecutor(firstExecutor) {
        // the block immediately hops to the expected executor
        dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
        dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
        print("OK: withTaskExecutor { withTaskExecutor withTaskExecutor { { ... } } }")
        await nonisolatedAsyncMethod(expectedOn: firstExecutor)
      }
    }
  }

  MainActor.preconditionIsolated()
  dispatchPrecondition(condition: .onQueue(DispatchQueue.main))
}

func testDisablingTaskExecutorPreference(_ firstExecutor: MyTaskExecutor,
                                         _ secondExecutor: MyTaskExecutor) async {
  dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
  dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))

  await withTaskExecutor(firstExecutor) {
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    await withTaskExecutor(nil) {
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
      print("OK: withTaskExecutor(nil) { ... }")
    }
  }
}

@main struct Main {

  static func main() async {
    let firstExecutor = MyTaskExecutor(queue: DispatchQueue(label: "first"))
    let secondExecutor = MyTaskExecutor(queue: DispatchQueue(label: "second"))

    // === nonisolated func
    await Task(on: firstExecutor) {
      await nonisolatedAsyncMethod(expectedOn: firstExecutor)
    }.value

    // We properly hop back to the main executor from the nonisolated func which used a a task executor
    MainActor.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(DispatchQueue.main))

    await testNestingWithExecutor(firstExecutor, secondExecutor)

//    await testDisablingTaskExecutorPreference(firstExecutor, secondExecutor)
  }
}