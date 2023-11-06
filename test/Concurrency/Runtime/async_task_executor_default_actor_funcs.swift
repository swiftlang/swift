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
      fatalError("NEIN")
      print("\(self): run job on queue")
      job.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

actor ThreaddyTheDefaultActor {
  func actorIsolated(expectedExecutor: NaiveQueueExecutor) async {
    dispatchPrecondition(condition: .onQueue(expectedExecutor.queue))
    expectedExecutor.preconditionIsolated()
  }
}

@main struct Main {

  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let queue = DispatchQueue(label: "example-queue")
      let executor = NaiveQueueExecutor(queue)

      let defaultActor = ThreaddyTheDefaultActor()

      await Task(on: executor) {
        await defaultActor.actorIsolated(expectedExecutor: executor)
      }.value

//      await withTaskExecutor(executor) {
//        await defaultActor.actorIsolated(expectedExecutor: executor)
//      }
    }
  }
}
