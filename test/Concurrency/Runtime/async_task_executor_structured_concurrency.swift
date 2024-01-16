// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// rdar://120430239
// UNSUPPORTED: CPU=arm64e

import Dispatch
import StdlibUnittest

import Dispatch

final class MyTaskExecutor: _TaskExecutor, @unchecked Sendable {
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

func testTaskGroup(_ firstExecutor: MyTaskExecutor,
                   _ secondExecutor: MyTaskExecutor) async {
  // 1 level of child tasks
  await withTaskGroup(of: Int.self) { group in
    group._addTask(on: firstExecutor) {
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
      return 1
    }
  }
  await withThrowingTaskGroup(of: Int.self) { group in
    group._addTask(on: firstExecutor) {
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
      return 2
    }
  }
  await withDiscardingTaskGroup() { group in
    group._addTask(on: firstExecutor) {
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    }
  }
  try! await withThrowingDiscardingTaskGroup() { group in
    group._addTask(on: firstExecutor) {
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
    }
  }

  // Multiple levels of child tasks
  await withTaskGroup(of: Int.self) { group in
    group._addTask(on: firstExecutor) {
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))

      return await withTaskGroup(of: Int.self) { group in
        group._addTask(on: secondExecutor) {
          dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
          dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
          return 12
        }
        return await group.next()!
      }
    }
  }

  // Disabling task preference, in task group child task
  _ = await withTaskGroup(of: Int.self) { group in
    dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
    dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))

    group._addTask(on: secondExecutor) {
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      return await withTaskGroup(of: Int.self) { inner in
        inner._addTask(on: nil) {
          // disabled the preference
          dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
          dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
          return 42
        }
        return await inner.next()!
      }
    }

    _ = group._addTaskUnlessCancelled(on: secondExecutor) {
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      return await withTaskGroup(of: Int.self) { inner in
        inner._addTask(on: nil) {
          // disabled the preference
          dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
          dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
          return 42
        }
        return await inner.next()!
      }
    }

    group._addTask(on: secondExecutor) {
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      await withDiscardingTaskGroup { inner in
        inner._addTask(on: nil) {
          // disabled the preference
          dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
          dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
        }
        _ = inner._addTaskUnlessCancelled(on: nil) {
          // disabled the preference
          dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
          dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
        }
      }
      return 0
    }

    group._addTask(on: secondExecutor) {
      dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
      try! await withThrowingDiscardingTaskGroup { inner in
        inner._addTask(on: nil) {
          // disabled the preference
          dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
          dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
        }
        _ = inner._addTaskUnlessCancelled(on: nil) {
          // disabled the preference
          dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
          dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
        }
      }
      return 0
    }

    return await group.next()!
  }
}

func testAsyncLet(_ firstExecutor: MyTaskExecutor,
                  _ secondExecutor: MyTaskExecutor) async {
  await _withTaskExecutor(firstExecutor) {
    async let first: Int = {
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))
      return 12
    }()
    _ = await first
  }
  await _withTaskExecutor(firstExecutor) {
    await _withTaskExecutor(secondExecutor) {
      async let first: Int = {
        dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
        dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
        return 12
      }()
      _ = await first
    }
  }

  await _withTaskExecutor(firstExecutor) {
    await _withTaskExecutor(secondExecutor) {
      async let first: Int = {
        async let firstInside: Int = {
          dispatchPrecondition(condition: .notOnQueue(firstExecutor.queue))
          dispatchPrecondition(condition: .onQueue(secondExecutor.queue))
          return 12
        }()
        return await firstInside
      }()
      _ = await first
    }
  }
}

func testGroupAsyncLet(_ firstExecutor: MyTaskExecutor,
                       _ secondExecutor: MyTaskExecutor) async {
  await withTaskGroup(of: Void.self) { group in
    group._addTask(on: firstExecutor) {
      dispatchPrecondition(condition: .onQueue(firstExecutor.queue))
      dispatchPrecondition(condition: .notOnQueue(secondExecutor.queue))

      async let first: () = expect(firstExecutor)
      _ = await first

      await _withTaskExecutor(secondExecutor) {
        async let second: () = expect(secondExecutor)
        _ = await second
      }
    }
  }
}

func expect(_ expected: MyTaskExecutor) {
  dispatchPrecondition(condition: .onQueue(expected.queue))
}

@main struct Main {

  static func main() async {
    let firstExecutor = MyTaskExecutor(queue: DispatchQueue(label: "first"))
    let secondExecutor = MyTaskExecutor(queue: DispatchQueue(label: "second"))

    await testTaskGroup(firstExecutor, secondExecutor)

    await testAsyncLet(firstExecutor, secondExecutor)

    await testGroupAsyncLet(firstExecutor, secondExecutor)
  }
}
