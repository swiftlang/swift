// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch
import StdlibUnittest

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

  func enqueue(_ job: consuming ExecutorJob) {
    enqueueCount += 1
    print("enqueue job")
  }
}



@main struct Main {

  static func main() async {
    let countingExecutor = CountEnqueuesTaskExecutor()

    var expectedEnqueues = 0

    // ==== Task Group
    do {
      await withTaskGroup(of: Int.self) { group in
        group.addTask(on: countingExecutor) {
          fatalError("never executed")
        }
      }
      expectedEnqueues += 1
      countingExecutor.expectEnqueues(expectedEnqueues)

      await withThrowingTaskGroup(of: Int.self) { group in
        group.addTask(on: countingExecutor) {
          fatalError("never executed")
        }
      }
      expectedEnqueues += 1
      countingExecutor.expectEnqueues(expectedEnqueues)

      await withDiscardingTaskGroup(of: Int.self) { group in
        group.addTask(on: countingExecutor) {
          fatalError("never executed")
        }
      }
      expectedEnqueues += 1
      countingExecutor.expectEnqueues(expectedEnqueues)

      await withThrowingDiscardingTaskGroup(of: Int.self) { group in
        group.addTask(on: countingExecutor) {
          fatalError("never executed")
        }
      }
      expectedEnqueues += 1
      countingExecutor.expectEnqueues(expectedEnqueues)
    }
  }
