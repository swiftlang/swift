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

final class CountEnqueuesSerialExecutor: SerialExecutor, CountingExecutor, @unchecked Sendable {
  var enqueueCount = 0
  let queue: DispatchQueue

  init(queue: DispatchQueue) {
    self.queue = queue
  }

  func enqueue(_ job: consuming ExecutorJob) {
    enqueueCount += 1
    let job = UnownedJob(job)
    queue.async {
      job.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

final class CountEnqueuesExecutor: CountingExecutor, @unchecked Sendable {
  var enqueueCount = 0

  func enqueue(_ job: consuming ExecutorJob) {
    enqueueCount += 1
    print("enqueue job")
  }
}


func nonisolatedAsyncMethod(expect executor: CountEnqueuesSerialExecutor) async {
  executor.preconditionIsolated()
  dispatchPrecondition(condition: .onQueue(executor.queue))

  print("Hello there")
}

@main struct Main {

  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let queue = DispatchQueue(label: "sample")
      let countingSerialExecutor = CountEnqueuesSerialExecutor(queue: queue)

      await withExecutor(countingSerialExecutor) {
        // the block immediately hops to the expected executor
        countingSerialExecutor.preconditionIsolated()
        dispatchPrecondition(condition: .onQueue(countingSerialExecutor.queue))
        print("OK: withExecutor body")
      }

      // A nonisolated async func must run on the expected executor,
      // rather than on the default global pool
      await withExecutor(countingSerialExecutor) {
        await nonisolatedAsyncMethod(expect: countingSerialExecutor)
        print("OK: nonisolated async func")
      }

      // child tasks must be started on the same executor
      await withExecutor(countingSerialExecutor) {

      }

    }
  }
}