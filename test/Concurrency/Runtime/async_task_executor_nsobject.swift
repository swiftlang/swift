// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library )
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library  -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

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
@_spi(ExperimentalScheduling) import _Concurrency

import Foundation
import Darwin


/// Delayed jobs that haven't run or been cancelled yet.  Whoever removes a job
/// from here owns it, which is how the timer and `cancel` decide between them.
final class PendingJobs: @unchecked Sendable {
  private let queue = DispatchQueue(label: "PendingJobs")
  private var jobs: [UInt64: UnownedJob] = [:]
  private var nextID: UInt64 = 1

  func add(_ job: UnownedJob) -> UInt64 {
    return queue.sync {
      let id = nextID
      nextID += 1
      jobs[id] = job
      return id
    }
  }

  func take(_ id: UInt64) -> UnownedJob? {
    return queue.sync { jobs.removeValue(forKey: id) }
  }
}

// Sneaky trick to make this type objc reference counted.
//
// This test specifically checks that our reference counting accounts for existence of
// objective-c types as TaskExecutors -- which was a bug where we'd swift_release
// obj-c excecutors by accident (rdar://131151645).
final class NSQueueTaskExecutor: NSData, TaskExecutor, SchedulingExecutor, @unchecked Sendable {
  let pending = PendingJobs()

  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    DispatchQueue.main.async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }

  public func supportsScheduling(on clock: SystemClockID) -> Bool {
    return true
  }

  public func schedule<C: Clock>(_ _job: consuming ExecutorJob,
                                 at fireTime: SchedulingExecutorFireTime<C>,
                                 tolerance: C.Duration?,
                                 clock: C) -> JobCancellationToken {
    // Convert to `Swift.Duration`
    let duration = fireTime.asDuration(clock: clock) as! Swift.Duration

    // Now turn that into nanoseconds
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000

    // Get a Dispatch time
    let deadline = DispatchTime.now().advanced(
      by: .seconds(Int(seconds))
    ).advanced(
      by: .nanoseconds(Int(nanoseconds))
    )

    let id = pending.add(UnownedJob(_job))
    DispatchQueue.main.asyncAfter(deadline: deadline) {
      if let job = self.pending.take(id) {
        job.runSynchronously(on: self.asUnownedTaskExecutor())
      }
    }

    return JobCancellationToken(canceller: self, opaqueData: [Int(id), 0])
  }

  // We can't remove jobs from `asyncAfter`, but we can make its closure do
  // nothing, and run the job now instead.
  public func cancel(jobWithToken token: borrowing JobCancellationToken) -> Bool {
    guard let job = pending.take(UInt64(token.opaqueData[0])) else {
      return false
    }
    if job.isResumption {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    } else {
      enqueue(ExecutorJob(job))
    }
    return true
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
