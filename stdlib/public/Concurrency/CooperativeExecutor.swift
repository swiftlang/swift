//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

import Swift

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
/// A wait queue is a specialised priority queue used to run a timer.
@available(StdlibDeploymentTarget 6.3, *)
struct WaitQueue {
  /// A job waiting for its deadline.
  ///
  /// We keep the deadline and an ID alongside the job, rather than in the
  /// job's executor private data, so that a cancelled job can be handed back
  /// exactly as we received it, and so that a stale cancellation can never
  /// match a different job that happens to reuse the same address.
  struct Entry {
    var job: UnownedJob
    var deadline: CooperativeExecutor.Timestamp
    var id: UInt64
  }

  var queue: PriorityQueue<Entry>
  var clock: _ClockID

  init(clock: _ClockID) {
    queue = PriorityQueue(compare: { $0.deadline < $1.deadline })
    self.clock = clock
  }

  var currentTime: CooperativeExecutor.Timestamp {
    var now: CooperativeExecutor.Timestamp = .zero
    unsafe _getTime(seconds: &now.seconds,
                    nanoseconds: &now.nanoseconds,
                    clock: clock.rawValue)
    return now
  }

  mutating func enqueue(_ job: consuming ExecutorJob,
                        after delay: CooperativeExecutor.Duration,
                        id: UInt64) {
    let deadline = currentTime + delay
    queue.push(Entry(job: UnownedJob(job), deadline: deadline, id: id))
  }

  mutating func forEachReadyJob(body: (consuming ExecutorJob) -> ()) {
    let now = currentTime
    while let entry = queue.pop(when: { $0.deadline < now }) {
      body(ExecutorJob(entry.job))
    }
  }

  /// Remove the job with the specified ID, if it is still waiting.
  mutating func cancel(id: UInt64) -> ExecutorJob? {
    if let entry = queue.removeFirst(where: { $0.id == id }) {
      return ExecutorJob(entry.job)
    }
    return nil
  }

  var timeToNextJob: CooperativeExecutor.Duration? {
    if let entry = queue.top {
      let now = currentTime
      if entry.deadline > now {
        return entry.deadline - now
      } else {
        return CooperativeExecutor.Duration(seconds: 0, nanoseconds: 0)
      }
    }
    return nil
  }
}
#endif

/// A co-operative executor that can be used as the main executor or as a
/// task executor.
@available(StdlibDeploymentTarget 6.3, *)
final class CooperativeExecutor: Executor, @unchecked Sendable {
  var runQueue: PriorityQueue<UnownedJob>
  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  var suspendingWaitQueue = WaitQueue(clock: .suspending)
  var continuousWaitQueue = WaitQueue(clock: .continuous)

  /// The ID to give to the next scheduled job; used to identify jobs for
  /// cancellation.  IDs are never reused.
  var nextJobID: UInt64 = 1
  #endif
  var shouldStop: Bool = false

  /// Internal representation of a duration for CooperativeExecutor
  struct Duration: Comparable {
    var seconds: Int64
    var nanoseconds: Int64

    init(seconds: Int64, nanoseconds: Int64) {
      self.seconds = seconds
      self.nanoseconds = nanoseconds
    }

    init(from duration: Swift.Duration) {
      let (seconds, attoseconds) = duration.components
      self.seconds = seconds
      self.nanoseconds = attoseconds / 1_000_000_000
    }

    static func == (lhs: Duration, rhs: Duration) -> Bool {
      return lhs.seconds == rhs.seconds && lhs.nanoseconds == rhs.nanoseconds
    }
    static func < (lhs: Duration, rhs: Duration) -> Bool {
      return lhs.seconds < rhs.seconds || (
        lhs.seconds == rhs.seconds
          && lhs.nanoseconds < rhs.nanoseconds
      )
    }
  }

  /// Internal representation of a timestamp for CooperativeExecutor
  struct Timestamp: Comparable {
    var seconds: Int64
    var nanoseconds: Int64

    static var zero: Timestamp {
      return Timestamp(seconds: 0, nanoseconds: 0)
    }

    static func == (lhs: Timestamp, rhs: Timestamp) -> Bool {
      return lhs.seconds == rhs.seconds && lhs.nanoseconds == rhs.nanoseconds
    }
    static func < (lhs: Timestamp, rhs: Timestamp) -> Bool {
      return lhs.seconds < rhs.seconds || (
        lhs.seconds == rhs.seconds
          && lhs.nanoseconds < rhs.nanoseconds
      )
    }
    static func - (lhs: Timestamp, rhs: Timestamp) -> Duration {
      if lhs.nanoseconds < rhs.nanoseconds {
        return Duration(seconds: lhs.seconds - rhs.seconds - 1,
                        nanoseconds: 1_000_000_000 + lhs.nanoseconds
                          - rhs.nanoseconds)
      }
      return Duration(seconds: lhs.seconds - rhs.seconds,
                      nanoseconds: lhs.nanoseconds - rhs.nanoseconds)
    }
    static func + (lhs: Timestamp, rhs: Duration) -> Timestamp {
      var seconds = lhs.seconds + rhs.seconds
      var nanoseconds = lhs.nanoseconds + rhs.nanoseconds
      // Normally will run only once
      while nanoseconds > 1_000_000_000 {
        seconds += 1
        nanoseconds -= 1_000_000_000
      }
      return Timestamp(seconds: seconds, nanoseconds: nanoseconds)
    }
  }

  public init() {
    runQueue = PriorityQueue(compare: { $0.priority > $1.priority })
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    runQueue.push(UnownedJob(job))
  }

  public var isMainExecutor: Bool { true }
}

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(StdlibDeploymentTarget 6.3, *)
extension CooperativeExecutor: SchedulingExecutor {

  func currentTime(clock: _ClockID) -> Timestamp {
    var now: Timestamp = .zero
    unsafe _getTime(seconds: &now.seconds,
                    nanoseconds: &now.nanoseconds,
                    clock: clock.rawValue)
    return now
  }

  // The values stored in `JobCancellationToken.opaqueData.0`
  private static var continuousClockToken: Int { 1 }
  private static var suspendingClockToken: Int { 2 }

  // Note that, like `enqueue(_:)`, none of this is thread-safe; the
  // cooperative executor is intended to be driven from a single thread, and
  // `cancel` must be called from that thread too.
  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                run: SchedulingExecutorFireTime<C>,
                                tolerance: C.Duration?,
                                clock: C) -> JobCancellationToken {
    let id = nextJobID
    nextJobID += 1

    // If it's a clock we know, get the duration to wait
    if let _ = clock as? ContinuousClock {
      let continuousDuration =
        run.asDuration(clock: clock) as! ContinuousClock.Duration
      continuousWaitQueue.enqueue(job, after: Duration(from: continuousDuration),
                                  id: id)
      return JobCancellationToken(
        executor: self, jobID: id,
        opaqueData: (Self.continuousClockToken, 0))
    } else if let _ = clock as? SuspendingClock {
      let suspendingDuration =
        run.asDuration(clock: clock) as! SuspendingClock.Duration
      suspendingWaitQueue.enqueue(job, after: Duration(from: suspendingDuration),
                                  id: id)
      return JobCancellationToken(
        executor: self, jobID: id,
        opaqueData: (Self.suspendingClockToken, 0))
    } else {
      fatalError("Sorry, cannot schedule on an unknown clock")
    }
  }

  public func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    switch token.opaqueData.0 {
    case Self.continuousClockToken:
      return continuousWaitQueue.cancel(id: token.jobID)
    case Self.suspendingClockToken:
      return suspendingWaitQueue.cancel(id: token.jobID)
    default:
      return nil
    }
  }

}
#endif

@available(StdlibDeploymentTarget 6.3, *)
extension CooperativeExecutor: RunLoopExecutor {
  public func run() throws {
    try runUntil { false }
  }

  public func runUntil(_ condition: () -> Bool) throws {
    shouldStop = false
    while !shouldStop && !condition() {
      #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
      // Process the timer queues
      suspendingWaitQueue.forEachReadyJob {
        runQueue.push(UnownedJob($0))
      }
      continuousWaitQueue.forEachReadyJob {
        runQueue.push(UnownedJob($0))
      }
      #endif

      // Now run any queued jobs
      var runQ = runQueue.take()
      while let job = runQ.pop() {
        unsafe ExecutorJob(job).runSynchronously(
          on: self.asUnownedSerialExecutor()
        )
      }

      #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
      // Finally, wait until the next deadline
      var toWait: Duration? = suspendingWaitQueue.timeToNextJob

      if let continuousToWait = continuousWaitQueue.timeToNextJob {
        if toWait == nil ||  continuousToWait < toWait! {
          toWait = continuousToWait
        }
      }

      if let toWait {
        _sleep(seconds: toWait.seconds,
               nanoseconds: toWait.nanoseconds)
      } else if runQueue.isEmpty {
        // Stop if no more jobs are available
        break
      }
      #else // $Embedded || SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
      if runQueue.isEmpty {
        // Stop if no more jobs are available
        break
      }
      #endif
    }
  }

  public func stop() {
    shouldStop = true
  }
}

@available(StdlibDeploymentTarget 6.3, *)
extension CooperativeExecutor: SerialExecutor {}

@available(StdlibDeploymentTarget 6.3, *)
extension CooperativeExecutor: TaskExecutor {}

@available(StdlibDeploymentTarget 6.3, *)
extension CooperativeExecutor: MainExecutor {}

#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
