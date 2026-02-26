// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library )
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library  -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch
@_spi(ConcurrencyExecutors) import _Concurrency

func p(_ message: String, file: String = #fileID, line: Int = #line) {
  print("[\(file):\(line)] \(message)")
}

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
    p("OK: withTaskExecutor body")
    await nonisolatedAsyncMethod(expectedOn: firstExecutor)
  }

  await withTaskExecutorPreference(firstExecutor) {
    await withTaskExecutorPreference(secondExecutor) {
      // the block immediately hops to the expected executor
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      p("OK: withTaskExecutor { withTaskExecutor { ... } }")
      await nonisolatedAsyncMethod(expectedOn: secondExecutor)
    }
  }

  p("withTaskExecutorPreference(first) before")
  await withTaskExecutorPreference(firstExecutor) {
    p("withTaskExecutorPreference(first) inside")
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    p("withTaskExecutorPreference(first) - withTaskExecutorPreference(second) before")
    await withTaskExecutorPreference(secondExecutor) {
      p("withTaskExecutorPreference(first) - withTaskExecutorPreference(second) inside")
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      p("withTaskExecutorPreference(first) - withTaskExecutorPreference(second) - withTaskExecutorPreference(first) before")
      await withTaskExecutorPreference(firstExecutor) {
        p("withTaskExecutorPreference(first) - withTaskExecutorPreference(second) - withTaskExecutorPreference(first) inside")
        // the block immediately hops to the expected executor
        dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
        dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
        p("OK: withTaskExecutor { withTaskExecutor withTaskExecutor { { ... } } }")
        await nonisolatedAsyncMethod(expectedOn: firstExecutor)
        p("withTaskExecutorPreference(first) - withTaskExecutorPreference(second) - withTaskExecutorPreference(first) leave ...")
      }
      p("withTaskExecutorPreference(first) - withTaskExecutorPreference(second) - withTaskExecutorPreference(first) after")
      p("OK: withTaskExecutor { withTaskExecutor { ... } }")
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      p("OK ...")
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      p("withTaskExecutorPreference(first) - withTaskExecutorPreference(second) leave ...")
    }
    p("withTaskExecutorPreference(first) - withTaskExecutorPreference(second) after")
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    p("OK ...")
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    p("withTaskExecutorPreference(first) leave ...")
  }
  p("withTaskExecutorPreference(first) after")

  p("Overhang!")
  dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
  p("OK ...")
  dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
  p("OK ...")
}

func testNestingWithExecutorNonisolated(_ firstExecutor: MyTaskExecutor,
                                        _ secondExecutor: MyTaskExecutor) async {
  await withTaskExecutorPreference(firstExecutor) {
    // the block immediately hops to the expected executor
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    p("OK: withTaskExecutor body")
    await nonisolatedAsyncMethod(expectedOn: firstExecutor)
  }

  await withTaskExecutorPreference(firstExecutor) {
    await withTaskExecutorPreference(secondExecutor) {
      // the block immediately hops to the expected executor
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      p("OK: withTaskExecutor { withTaskExecutor { ... } }")
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
        p("OK: withTaskExecutor { withTaskExecutor withTaskExecutor { { ... } } }")
        await nonisolatedAsyncMethod(expectedOn: firstExecutor)
      } // on first
      p("OK ...")
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      p("OK ...")
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      p("OK ...")
    } // on second
    p("OK ...")
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    p("OK ...")
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    p("OK ...")
  } // on first

  p("Disabled checks, we're overhanging now ...")
  dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
  dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
}

func testDisablingTaskExecutorPreference(_ firstExecutor: MyTaskExecutor,
                                         _ secondExecutor: MyTaskExecutor) async {
  // FIXME: overhang!!!!
  p("Overhang...!!!")
  dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
  p("OK ...")
  dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))

  await withTaskExecutorPreference(firstExecutor) {
    p("OK ...")
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    p("OK ...")
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    await withTaskExecutorPreference(globalConcurrentExecutor) {
      p("OK ...")
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      p("OK ...")
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
      p("OK: withTaskExecutorPreference(globalConcurrentExecutor) { ... }")
    } // on second
    await withTaskExecutorPreference(nil) { // no specific preference == okey to inherit
      p("OK ...")
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      p("OK ...")
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
      p("OK: withTaskExecutorPreference(nil) { ... }")
    } // on second
    p("OK ...")
    dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
    p("OK ...")
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
      p("OK ...")
      precondition(currentTaskExecutor == firstExecutor.asUnownedTaskExecutor())
      p("OK: currentTaskExecutor == firstExecutor.asUnownedTaskExecutor()")
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
