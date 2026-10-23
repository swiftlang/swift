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
  /// What to do when an entry's deadline arrives.
  @safe
  enum Payload {
    /// Run a job.
    case job(UnownedJob)

    /// Resume a sleeping task.
    case continuation(UnsafeContinuation<Void, CancellationError>)
  }

  /// A job or continuation waiting for its deadline.
  ///
  /// We keep the deadline and an ID alongside the payload, rather than in the
  /// job's executor private data, so that a cancelled job can be handed back
  /// exactly as we received it, and so that a stale cancellation can never
  /// match a different entry that happens to reuse the same address.
  struct Entry {
    var payload: Payload
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
    queue.push(Entry(payload: .job(UnownedJob(job)), deadline: deadline, id: id))
  }

  mutating func enqueue(_ continuation: consuming TimerContinuation,
                        after delay: CooperativeExecutor.Duration,
                        id: UInt64) {
    let deadline = currentTime + delay
    queue.push(Entry(payload: unsafe .continuation(continuation.continuation),
                     deadline: deadline, id: id))
  }

  mutating func forEachReady(job: (consuming ExecutorJob) -> (),
                             continuation: (consuming TimerContinuation) -> ()) {
    let now = currentTime
    while let entry = queue.pop(when: { $0.deadline < now }) {
      switch entry.payload {
      case let .job(unowned):
        job(ExecutorJob(unowned))
      case let .continuation(c):
        continuation(unsafe TimerContinuation(c))
      }
    }
  }

  /// Remove the entry with the specified ID, if it is still waiting.
  mutating func cancel(id: UInt64) -> Payload? {
    return queue.removeFirst(where: { $0.id == id })?.payload
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
///
/// It runs everything on whichever thread calls `run()` or `runUntil(_:)`, and
/// is not thread-safe: jobs must be enqueued, and timers cancelled, from that
/// thread.  It is exposed as SPI so that it can be tested.
@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
public final class CooperativeExecutor: Executor, @unchecked Sendable {
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
@_spi(ExperimentalCustomExecutors)
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

  /// Returns `true` if `clock` is one of the clocks that we schedule ourselves.
  private func isKnown<C: Clock>(_ clock: C) -> Bool {
    return clock is ContinuousClock || clock is SuspendingClock
  }

  /// Work out how long to wait on a clock that we know about, and which of
  /// our wait queues to use (as the value to store in the token).
  private func delay<C: Clock>(run: SchedulingExecutorFireTime<C>, clock: C)
    -> (delay: Duration, clockToken: Int) {
    if let _ = clock as? ContinuousClock {
      let duration = run.asDuration(clock: clock) as! ContinuousClock.Duration
      return (Duration(from: duration), Self.continuousClockToken)
    } else if let _ = clock as? SuspendingClock {
      let duration = run.asDuration(clock: clock) as! SuspendingClock.Duration
      return (Duration(from: duration), Self.suspendingClockToken)
    }
    fatalError("Sorry, cannot schedule on an unknown clock")
  }

  // Note that, like `enqueue(_:)`, none of this is thread-safe; the
  // cooperative executor is intended to be driven from a single thread, and
  // `cancel` must be called from that thread too.
  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                run: SchedulingExecutorFireTime<C>,
                                tolerance: C.Duration?,
                                clock: C) -> JobCancellationToken {
    // A clock we don't recognise may know how to schedule the job itself.
    if !isKnown(clock) && _isEnqueueingClock(clock) {
      return _enqueueUsingClock(job, on: self, run: run,
                                tolerance: tolerance, clock: clock)
    }

    let id = nextJobID
    nextJobID += 1

    let (delay, clockToken) = delay(run: run, clock: clock)
    if clockToken == Self.continuousClockToken {
      continuousWaitQueue.enqueue(job, after: delay, id: id)
    } else {
      suspendingWaitQueue.enqueue(job, after: delay, id: id)
    }
    return JobCancellationToken(canceller: self, jobID: id,
                                opaqueData: (clockToken, 0))
  }

  public func enqueue<C: Clock>(resuming continuation: consuming TimerContinuation,
                                run: SchedulingExecutorFireTime<C>,
                                tolerance: C.Duration?,
                                clock: C) -> JobCancellationToken {
    if !isKnown(clock) && _isEnqueueingClock(clock) {
      return _resumeUsingClock(continuation, run: run,
                               tolerance: tolerance, clock: clock)
    }

    let id = nextJobID
    nextJobID += 1

    let (delay, clockToken) = delay(run: run, clock: clock)
    if clockToken == Self.continuousClockToken {
      continuousWaitQueue.enqueue(continuation, after: delay, id: id)
    } else {
      suspendingWaitQueue.enqueue(continuation, after: delay, id: id)
    }
    return JobCancellationToken(canceller: self, jobID: id,
                                opaqueData: (clockToken, 0))
  }

  private func cancel(token: borrowing JobCancellationToken)
    -> WaitQueue.Payload? {
    switch token.opaqueData.0 {
    case Self.continuousClockToken:
      return continuousWaitQueue.cancel(id: token.jobID)
    case Self.suspendingClockToken:
      return suspendingWaitQueue.cancel(id: token.jobID)
    default:
      return nil
    }
  }

  public func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    if case let .job(job) = cancel(token: token) {
      return ExecutorJob(job)
    }
    return nil
  }

  public func cancel(continuationWithToken token: consuming JobCancellationToken)
    -> TimerContinuation? {
    if case let .continuation(continuation) = cancel(token: token) {
      return unsafe TimerContinuation(continuation)
    }
    return nil
  }

}
#endif

@_spi(ExperimentalCustomExecutors)
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
      suspendingWaitQueue.forEachReady(
        job: { runQueue.push(UnownedJob($0)) },
        continuation: { $0.resume() })
      continuousWaitQueue.forEachReady(
        job: { runQueue.push(UnownedJob($0)) },
        continuation: { $0.resume() })
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

      if !runQueue.isEmpty {
        // There is more to run, so don't wait for a timer
      } else if let toWait {
        _sleep(seconds: toWait.seconds,
               nanoseconds: toWait.nanoseconds)
      } else {
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

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
extension CooperativeExecutor: SerialExecutor {}

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
extension CooperativeExecutor: TaskExecutor {}

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
extension CooperativeExecutor: MainExecutor {}

#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
