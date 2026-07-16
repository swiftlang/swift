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

/// A co-operative executor that can be used as the main executor or as a
/// task executor.
@available(StdlibDeploymentTarget 9999, *)
final class CooperativeExecutor: Executor, @unchecked Sendable {
  var runQueue: PriorityQueue<UnownedJob>
  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  // Per-suspension delivery state for in-flight sleeps, keyed by the token id
  // minted in `arm`.  Holds the raw continuation handle so fire and cancel can
  // reconstruct the concrete `ExecutorContinuation<Void, CancellationError>` and
  // resume it.  Deadline ordering lives in the per-clock sleep heaps below.
  @safe
  enum SleepWork {
    case continuation(Builtin.RawUnsafeContinuation)
    case job(UnownedJob)
  }
  @safe
  struct SleepEntry {
    var work: SleepWork
    var onContinuous: Bool
    // Set by `cancel`: the sleep is resolved by resuming its continuation
    // *throwing* CancellationError rather than returning success.  The resume
    // itself is still performed on the run-loop thread by `processReadySleeps`.
    var cancelled: Bool = false
  }
  struct SleepHeapEntry {
    var deadline: Timestamp
    var token: OperationExecutorRegistration
  }
  var sleepContinuations: [OperationExecutorRegistration: SleepEntry] = [:]
  var continuousSleepHeap =
    PriorityQueue<SleepHeapEntry>(compare: { $0.deadline < $1.deadline })
  var suspendingSleepHeap =
    PriorityQueue<SleepHeapEntry>(compare: { $0.deadline < $1.deadline })
  // Monotonic counter for minting tokens in `arm`.  The cooperative executor
  // drains on a single thread, so a plain increment suffices.
  var nextSleepID: UInt64 = 0
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
@available(StdlibDeploymentTarget 9999, *)
extension CooperativeExecutor {

  func currentTime(clock: _ClockID) -> Timestamp {
    var now: Timestamp = .zero
    unsafe _getTime(seconds: &now.seconds,
                    nanoseconds: &now.nanoseconds,
                    clock: clock.rawValue)
    return now
  }
}

@available(StdlibDeploymentTarget 9999, *)
extension CooperativeExecutor: OperationExecutor {

  // Shared arming logic; `deadline` is already converted to this executor's
  // timestamp for the relevant clock.  Mints a fresh token, stores the
  // continuation handle or job keyed by it (so fire/cancel can reconstruct the
  // typed continuation to resume or run the job), and returns the token.
  // Removing the dict entry is the single-delivery "claim" (a stale heap entry
  // is skipped on fire via lazy deletion).
  private func arm(
    _ work: SleepWork,
    onContinuous: Bool,
    deadline: Timestamp
  ) -> OperationExecutorRegistration {
    nextSleepID &+= 1
    let token = OperationExecutorRegistration(id: nextSleepID)
    sleepContinuations[token] = SleepEntry(work: work, onContinuous: onContinuous)
    if onContinuous {
      continuousSleepHeap.push(SleepHeapEntry(deadline: deadline, token: token))
    } else {
      suspendingSleepHeap.push(SleepHeapEntry(deadline: deadline, token: token))
    }
    return token
  }

  // Called from the suspending function's cancellation handler, which runs on
  // the *cancelling* task's thread -- so it must NOT resume the continuation
  // here (executor continuations may only be resumed from the executor's own run
  // context).  Instead it flags the sleep cancelled and pushes a now-deadline
  // heap entry, so `processReadySleeps` wakes it on the run-loop thread and
  // resumes it throwing CancellationError.  A scheduled job is simply dropped.
  // A miss is safe: the sleep already fired/resolved, or the token is unknown.
  public func cancel(_ token: OperationExecutorRegistration) {
    guard var entry = sleepContinuations[token] else { return }
    switch entry.work {
    case .continuation:
      entry.cancelled = true
      sleepContinuations[token] = entry
      let now = currentTime(clock: entry.onContinuous ? .continuous : .suspending)
      let heapEntry = SleepHeapEntry(deadline: now, token: token)
      if entry.onContinuous {
        continuousSleepHeap.push(heapEntry)
      } else {
        suspendingSleepHeap.push(heapEntry)
      }
    case .job:
      // Dropping a scheduled job needs no resume.
      sleepContinuations.removeValue(forKey: token)
    }
  }

  public func escalatePriority(of token: OperationExecutorRegistration,
                               to newPriority: TaskPriority) {
    // The cooperative executor runs all work on its single run-loop thread at
    // the loop's own priority, so there is no per-continuation thread to
    // escalate.  Nothing to do.
  }
}

@available(StdlibDeploymentTarget 9999, *)
extension CooperativeExecutor: ContinuousClockExecutor {
  public func enqueue(
    _ continuation: consuming ExecutorContinuation<Void, CancellationError>,
    at instant: ContinuousClock.Instant,
    tolerance: ContinuousClock.Duration?
  ) -> OperationExecutorRegistration {
    let delay = Duration(from: ContinuousClock().now.duration(to: instant))
    return arm(.continuation(continuation._takeContext()),
               onContinuous: true, deadline: currentTime(clock: .continuous) + delay)
  }

  public func enqueue(
    _ job: consuming ExecutorJob,
    at instant: ContinuousClock.Instant,
    tolerance: ContinuousClock.Duration?
  ) -> OperationExecutorRegistration {
    let delay = Duration(from: ContinuousClock().now.duration(to: instant))
    return arm(.job(UnownedJob(job)),
               onContinuous: true, deadline: currentTime(clock: .continuous) + delay)
  }
}

@available(StdlibDeploymentTarget 9999, *)
extension CooperativeExecutor: SuspendingClockExecutor {
  public func enqueue(
    _ continuation: consuming ExecutorContinuation<Void, CancellationError>,
    at instant: SuspendingClock.Instant,
    tolerance: SuspendingClock.Duration?
  ) -> OperationExecutorRegistration {
    let delay = Duration(from: SuspendingClock().now.duration(to: instant))
    return arm(.continuation(continuation._takeContext()),
               onContinuous: false, deadline: currentTime(clock: .suspending) + delay)
  }

  public func enqueue(
    _ job: consuming ExecutorJob,
    at instant: SuspendingClock.Instant,
    tolerance: SuspendingClock.Duration?
  ) -> OperationExecutorRegistration {
    let delay = Duration(from: SuspendingClock().now.duration(to: instant))
    return arm(.job(UnownedJob(job)),
               onContinuous: false, deadline: currentTime(clock: .suspending) + delay)
  }
}
#endif

@available(StdlibDeploymentTarget 9999, *)
extension CooperativeExecutor: RunLoopExecutor {
  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  // Resume every ready sleep on `heap` whose deadline has elapsed: a
  // continuation resumes with success, a job is pushed onto the run queue.
  // Removing the `sleepContinuations` entry is the single-delivery claim, so a
  // racing `cancel` finds nothing; stale heap entries (whose dict entry was
  // already claimed by cancel) are skipped.
  func processReadySleeps(_ heap: inout PriorityQueue<SleepHeapEntry>,
                          clock: _ClockID) {
    let now = currentTime(clock: clock)
    var dueContinuations: [Builtin.RawUnsafeContinuation] = []
    var cancelledContinuations: [Builtin.RawUnsafeContinuation] = []
    var dueJobs: [UnownedJob] = []
    while let top = heap.top, !(now < top.deadline) {
      _ = heap.pop()
      if let e = sleepContinuations.removeValue(forKey: top.token) {
        switch e.work {
        case .continuation(let context):
          if e.cancelled {
            cancelledContinuations.append(context)
          } else {
            dueContinuations.append(context)
          }
        case .job(let job): dueJobs.append(job)
        }
      }
    }
    for context in dueContinuations {
      let cont = ExecutorContinuation<Void, CancellationError>(context: context)
      cont.resumeSynchronously(returning: ())
    }
    for context in cancelledContinuations {
      let cont = ExecutorContinuation<Void, CancellationError>(context: context)
      cont.resumeSynchronously(throwing: CancellationError())
    }
    for job in dueJobs {
      runQueue.push(job)
    }
  }

  // Time until the earliest pending sleep on `heap`, or nil if empty.
  func timeToNextSleep(_ heap: PriorityQueue<SleepHeapEntry>,
                       clock: _ClockID) -> Duration? {
    guard let top = heap.top else { return nil }
    let now = currentTime(clock: clock)
    if top.deadline > now {
      return top.deadline - now
    }
    return Duration(seconds: 0, nanoseconds: 0)
  }
  #endif

  public func run() throws {
    try runUntil { false }
  }

  public func runUntil(_ condition: () -> Bool) throws {
    shouldStop = false
    while !shouldStop && !condition() {
      #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
      // Process ready sleep continuations.  `resumeSynchronously` runs the
      // resumed task inline on this thread when compatible, else enqueues it
      // back onto this executor's run queue.
      processReadySleeps(&continuousSleepHeap, clock: .continuous)
      processReadySleeps(&suspendingSleepHeap, clock: .suspending)
      #endif

      // Now run any queued jobs
      var runQ = runQueue.take()
      while let job = runQ.pop() {
        unsafe ExecutorJob(job).runSynchronously(
          on: self.asUnownedSerialExecutor()
        )
      }

      #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
      // Finally, wait until the earliest pending sleep deadline.
      var toWait: Duration? = timeToNextSleep(continuousSleepHeap,
                                              clock: .continuous)

      for candidate in [timeToNextSleep(suspendingSleepHeap, clock: .suspending)] {
        if let candidate, toWait == nil || candidate < toWait! {
          toWait = candidate
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

@available(StdlibDeploymentTarget 9999, *)
extension CooperativeExecutor: SerialExecutor {}

@available(StdlibDeploymentTarget 9999, *)
extension CooperativeExecutor: TaskExecutor {}

@available(StdlibDeploymentTarget 9999, *)
extension CooperativeExecutor: MainExecutor {}

#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
