// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch
@_spi(ConcurrencyExecutors) import _Concurrency

final class MyTaskExecutor: TaskExecutor, @unchecked Sendable, CustomStringConvertible {
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

  var description: String {
    "\(Self.self)(\(ObjectIdentifier(self))"
  }
}

nonisolated func nonisolatedAsyncMethod(expectedOn executor: MyTaskExecutor) async {
  dispatchPrecondition(condition: .onQueue(executor.queue))
}

func testNestingWithExecutorMainActor(_ firstExecutor: MyTaskExecutor,
                                      _ secondExecutor: MyTaskExecutor) async {
  dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
  dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))

  await withTaskExecutorPreference(firstExecutor) {
    // the block immediately hops to the expected executor
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    print("OK: withTaskExecutor body")
    await nonisolatedAsyncMethod(expectedOn: firstExecutor)
  }

  await withTaskExecutorPreference(firstExecutor) {
    await withTaskExecutorPreference(secondExecutor) {
      // the block immediately hops to the expected executor
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      print("OK: withTaskExecutor { withTaskExecutor { ... } }")
      await nonisolatedAsyncMethod(expectedOn: secondExecutor)
    }
  }

  await withTaskExecutorPreference(firstExecutor) {
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    await withTaskExecutorPreference(secondExecutor) {
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      await withTaskExecutorPreference(firstExecutor) {
        // the block immediately hops to the expected executor
        dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
        dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
        print("OK: withTaskExecutor { withTaskExecutor withTaskExecutor { { ... } } }")
        await nonisolatedAsyncMethod(expectedOn: firstExecutor)
      }
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
    }
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
  }

  dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
  dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
}

func testNestingWithExecutorNonisolated(_ firstExecutor: MyTaskExecutor,
                                        _ secondExecutor: MyTaskExecutor) async {
  await withTaskExecutorPreference(firstExecutor) {
    // the block immediately hops to the expected executor
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    print("OK: withTaskExecutor body")
    await nonisolatedAsyncMethod(expectedOn: firstExecutor)
  }

  await withTaskExecutorPreference(firstExecutor) {
    await withTaskExecutorPreference(secondExecutor) {
      // the block immediately hops to the expected executor
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      print("OK: withTaskExecutor { withTaskExecutor { ... } }")
      await nonisolatedAsyncMethod(expectedOn: secondExecutor)
    }
  }

  await withTaskExecutorPreference(firstExecutor) {
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    await withTaskExecutorPreference(secondExecutor) {
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      await withTaskExecutorPreference(firstExecutor) {
        // the block immediately hops to the expected executor
        dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
        dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
        print("OK: withTaskExecutor { withTaskExecutor withTaskExecutor { { ... } } }")
        await nonisolatedAsyncMethod(expectedOn: firstExecutor)
      } // on first
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
    } // on second
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
  } // on first

  dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
  dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
}

func testDisablingTaskExecutorPreference(_ firstExecutor: MyTaskExecutor,
                                         _ secondExecutor: MyTaskExecutor) async {
  dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
  dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))

  await withTaskExecutorPreference(firstExecutor) {
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    await withTaskExecutorPreference(globalConcurrentExecutor) {
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
      print("OK: withTaskExecutorPreference(globalConcurrentExecutor) { ... }")
    } // on second
    await withTaskExecutorPreference(nil) { // no specific preference == okey to inherit
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
      print("OK: withTaskExecutorPreference(nil) { ... }")
    } // on second
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
  } // on first
}

func testGetCurrentTaskExecutor(_ firstExecutor: MyTaskExecutor,
                                _ secondExecutor: MyTaskExecutor) async {

  _ = await Task {
    withUnsafeCurrentTask { task in
      precondition(nil == task!.unownedTaskExecutor, "unexpected task executor value, should be nil")
    }
  }.value

  await withTaskExecutorPreference(firstExecutor) {
    withUnsafeCurrentTask { task in
      guard let task else {
        fatalError("Missing task?")
      }
      guard let currentTaskExecutor = task.unownedTaskExecutor else {
        fatalError("Expected to have task executor")
      }
      // Test that we can compare UnownedExecutors:
      precondition(currentTaskExecutor == firstExecutor.asUnownedTaskExecutor())
      print("OK: currentTaskExecutor == firstExecutor.asUnownedTaskExecutor()")
    }
  }
}

@main struct Main {

  static func main() async {
    let firstExecutor = MyTaskExecutor(queue: DispatchQueue(label: "first"))
    let secondExecutor = MyTaskExecutor(queue: DispatchQueue(label: "second"))

    // === nonisolated func
    await Task(executorPreference: firstExecutor) {
      await nonisolatedAsyncMethod(expectedOn: firstExecutor)
    }.value

    // We properly hop back to the main executor from the nonisolated func which used a a task executor
    MainActor.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(DispatchQueue.main))

    await testNestingWithExecutorMainActor(firstExecutor, secondExecutor)
    await testNestingWithExecutorNonisolated(firstExecutor, secondExecutor)

    await testDisablingTaskExecutorPreference(firstExecutor, secondExecutor)

    await testGetCurrentTaskExecutor(firstExecutor, secondExecutor)
  }
}
