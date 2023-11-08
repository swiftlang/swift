// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch
import StdlibUnittest

final class CountEnqueuesTaskExecutor: TaskExecutor, @unchecked Sendable {

  func enqueue(_ job: consuming ExecutorJob) {
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
