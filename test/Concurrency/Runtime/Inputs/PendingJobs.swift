import Dispatch

/// Delayed jobs that haven't run or been cancelled yet, for test executors that
/// can't take a job back from the timer that holds it.  Whoever removes a job
/// from here owns it, which is how the timer and `cancel` decide between them.
@available(SwiftStdlib 6.5, *)
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
