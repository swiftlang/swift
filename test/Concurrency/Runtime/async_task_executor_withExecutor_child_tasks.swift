// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch

protocol CountingExecutor: Executor, Sendable {
  var enqueueCount: Int { get }

  func expectEnqueues(_ expected: Int)
}

extension CountingExecutor {
  func expectEnqueues(_ expected: Int) {
    precondition(self.enqueueCount == expected, "Expected [\(expected)] enqueues but had [\(enqueueCount)] (difference: \(enqueueCount - expected))")
  }
}

final class CountEnqueuesTaskExecutor: TaskExecutor, CountingExecutor, @unchecked Sendable {
  var enqueueCount = 0
  let queue: DispatchQueue

  init(queue: DispatchQueue) {
    self.queue = queue
  }

  func enqueue(_ job: consuming ExecutorJob) {
    enqueueCount += 1
    let job = UnownedJob(job)
    queue.async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }
}

nonisolated func testFromNonisolated(_ countingTaskExecutor: CountEnqueuesTaskExecutor) async {
  await withTaskExecutor(countingTaskExecutor) {
    // countingTaskExecutor.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(countingTaskExecutor.queue))
    print("OK: withTaskExecutor body")
  }
}

@main struct Main {

  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let queue = DispatchQueue(label: "sample")
      let countingTaskExecutor = CountEnqueuesTaskExecutor(queue: queue)

      print("==== TaskGroup - respects task executor")
      MainActor.preconditionIsolated()
      await withTaskExecutor(countingTaskExecutor) {
        // the block immediately hops to the expected executor
//        countingTaskExecutor.preconditionIsolated()
//        dispatchPrecondition(condition: .onQueue(countingTaskExecutor.queue))

        await withTaskGroup(of: Int.self) { group in
          group.addTask {
            // countingTaskExecutor.preconditionIsolated()
            dispatchPrecondition(condition: .onQueue(countingTaskExecutor.queue))

            return 42
          }

          group.addTask(on: nil) {
            dispatchPrecondition(condition: .notOnQueue(countingTaskExecutor.queue))
            return 42
          }

          for await v in group {
            print("group: \(v)")
          }
        }
      }
      MainActor.preconditionIsolated()
//
      print("==== async let - respects task executor")
      await withTaskExecutor(countingTaskExecutor) {
        async let compute: Int = {
          // countingTaskExecutor.preconditionIsolated()
          dispatchPrecondition(condition: .onQueue(countingTaskExecutor.queue))
          return 42
        }()

        let x = await compute
        precondition(x == 42)
      }
      MainActor.preconditionIsolated()

//      print("==== Task{} - drops task executor")
//      await withTaskExecutor(countingTaskExecutor) {
//        let t = Task {
//          dispatchPrecondition(condition: .notOnQueue(countingTaskExecutor.queue))
//          return 42
//        }
//        let v = await t.value
//        precondition(v == 42)
//      }
//
//      print("==== Task.detached{} - drops task executor")
//      await withTaskExecutor(countingTaskExecutor) {
//        let t = Task.detached {
//          dispatchPrecondition(condition: .notOnQueue(countingTaskExecutor.queue))
//          return 42
//        }
//        let v = await t.value
//        precondition(v == 42)
//      }

      // TODO: implement handling Executor and not just TaskExecutor
    }
  }
}