// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library )

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import Dispatch
import StdlibUnittest
import _Concurrency

import Foundation
import Darwin

// Sneaky trick to make this type objc reference counted.
//
// This test specifically checks that our reference counting accounts for existence of
// objective-c types as TaskExecutors -- which was a bug where we'd swift_release
// obj-c excecutors by accident (rdar://131151645).
final class NSQueueTaskExecutor: NSData, TaskExecutor, SchedulableExecutor, @unchecked Sendable {
  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    DispatchQueue.main.async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }

  public func enqueue<C: Clock>(_ _job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    // Convert to `Swift.Duration`
    let duration = clock.convert(from: delay)!

    // Now turn that into nanoseconds
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000

    // Get a Dispatch time
    let deadline = DispatchTime.now().advanced(
      by: .seconds(Int(seconds))
    ).advanced(
      by: .nanoseconds(Int(nanoseconds))
    )

    let job = UnownedJob(_job)
    DispatchQueue.main.asyncAfter(deadline: deadline) {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }
}

@main struct Main {
  static func main() async {
    var taskExecutor: (any TaskExecutor)? = NSQueueTaskExecutor()

    let task = Task(executorPreference: taskExecutor) {
      dispatchPrecondition(condition: .onQueue(DispatchQueue.main))
      try? await Task.sleep(for: .seconds(2))
      return 12
    }

    taskExecutor = nil

    let num = await task.value
    assert(num == 12)
  }
}
