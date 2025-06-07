// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch
import StdlibUnittest
import _Concurrency

final class NaiveQueueExecutor: TaskExecutor {
  let queue: DispatchQueue

  init(_ queue: DispatchQueue) {
    self.queue = queue
  }

  public func enqueue(_ _job: consuming ExecutorJob) {
    print("\(self): enqueue")
    let job = UnownedJob(_job)
    queue.async {
      print("\(self): run job on queue")
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }
}

nonisolated func nonisolatedFunc(expectedExecutor: NaiveQueueExecutor) async {
  dispatchPrecondition(condition: .onQueue(expectedExecutor.queue))
}

@main struct Main {

  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let queue = DispatchQueue(label: "example-queue")
      let executor = NaiveQueueExecutor(queue)

      await Task(executorPreference: executor) {
        await nonisolatedFunc(expectedExecutor: executor)
      }.value
    }
  }
}
