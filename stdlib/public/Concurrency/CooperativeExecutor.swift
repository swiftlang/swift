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

// Store the Timestamp in the executor private data, if it will fit; otherwise,
// use the allocator to allocate space for it and stash a pointer in the private
// data area.
@available(StdlibDeploymentTarget 6.3, *)
extension ExecutorJob {
  fileprivate var cooperativeExecutorTimestampIsIndirect: Bool {
    return MemoryLayout<(Int, Int)>.size
      < MemoryLayout<CooperativeExecutor.Timestamp>.size
  }

  fileprivate var cooperativeExecutorTimestampPointer: UnsafeMutablePointer<CooperativeExecutor.Timestamp> {
    get {
      assert(cooperativeExecutorTimestampIsIndirect)
      return withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UnsafeMutablePointer<CooperativeExecutor.Timestamp>.self) {
          return unsafe $0[0]
        }
      }
    }
    set {
      assert(cooperativeExecutorTimestampIsIndirect)
      withUnsafeExecutorPrivateData {
        unsafe $0.withMemoryRebound(to: UnsafeMutablePointer<CooperativeExecutor.Timestamp>.self) {
          unsafe $0[0] = newValue
        }
      }
    }
  }

  fileprivate var cooperativeExecutorTimestamp: CooperativeExecutor.Timestamp {
    get {
      if cooperativeExecutorTimestampIsIndirect {
        let ptr = unsafe cooperativeExecutorTimestampPointer
        return unsafe ptr.pointee
      } else {
        return withUnsafeExecutorPrivateData {
          return unsafe $0.assumingMemoryBound(
            to: CooperativeExecutor.Timestamp.self
          )[0]
        }
      }
    }
    set {
      if cooperativeExecutorTimestampIsIndirect {
        let ptr = unsafe cooperativeExecutorTimestampPointer
        unsafe ptr.pointee = newValue
     } else {
        withUnsafeExecutorPrivateData {
          unsafe $0.withMemoryRebound(to: CooperativeExecutor.Timestamp.self) {
            unsafe $0[0] = newValue
          }
        }
      }
    }
  }

  fileprivate mutating func setupCooperativeExecutorTimestamp() {
    // If a Timestamp won't fit, allocate
    if cooperativeExecutorTimestampIsIndirect {
      let ptr: UnsafeMutablePointer<CooperativeExecutor.Timestamp>
      // Try to use the task allocator if it has one
      if let allocator {
        unsafe ptr = allocator.allocate(as: CooperativeExecutor.Timestamp.self)
      } else {
        unsafe ptr = .allocate(capacity: 1)
      }
      unsafe self.cooperativeExecutorTimestampPointer = ptr
    }
  }

  fileprivate mutating func clearCooperativeExecutorTimestamp() {
    // If a Timestamp won't fit, deallocate
    if cooperativeExecutorTimestampIsIndirect {
      let ptr = unsafe self.cooperativeExecutorTimestampPointer
      if let allocator {
        unsafe allocator.deallocate(ptr)
      } else {
        unsafe ptr.deallocate()
      }
    }
  }
}

/// A wait queue is a specialised priority queue used to run a timer.
@available(StdlibDeploymentTarget 6.3, *)
struct WaitQueue {
  var queue: PriorityQueue<UnownedJob>
  var clock: _ClockID

  init(clock: _ClockID) {
    queue = PriorityQueue(compare: {
                            ExecutorJob($0).cooperativeExecutorTimestamp
                              < ExecutorJob($1).cooperativeExecutorTimestamp
                          })
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
                        after delay: CooperativeExecutor.Duration) {
    let deadline = currentTime + delay
    job.setupCooperativeExecutorTimestamp()
    job.cooperativeExecutorTimestamp = deadline
    queue.push(UnownedJob(job))
  }

  mutating func forEachReadyJob(body: (consuming ExecutorJob) -> ()) {
    let now = currentTime
    while let job = queue.pop(
            when: {
              ExecutorJob($0).cooperativeExecutorTimestamp < now
            }) {
      var theJob = ExecutorJob(job)
      theJob.clearCooperativeExecutorTimestamp()
      body(theJob)
    }
  }

  var timeToNextJob: CooperativeExecutor.Duration? {
    if let job = queue.top {
      let deadline = ExecutorJob(job).cooperativeExecutorTimestamp
      let now = currentTime
      if deadline > now {
        return deadline - now
      } else {
        return CooperativeExecutor.Duration(seconds: 0, nanoseconds: 0)
      }
    }
    return nil
  }
}

/// A co-operative executor that can be used as the main executor or as a
/// task executor.
///
/// Thread-safety: this executor is `@unchecked Sendable` only because every
/// access to `runQueue` / `suspendingWaitQueue` / `continuousWaitQueue` happens
/// on the single thread that drives `runUntil`. The delayed-enqueue helpers
/// below are likewise only ever called synchronously from within that run loop
/// (via the `swift_task_enqueueGlobalWith{Delay,Deadline}` runtime hooks).
/// Driving this executor from multiple threads would race those collections.
@available(StdlibDeploymentTarget 6.3, *)
final class CooperativeExecutor: Executor, @unchecked Sendable {
  var runQueue: PriorityQueue<UnownedJob>
  var suspendingWaitQueue = WaitQueue(clock: .suspending)
  var continuousWaitQueue = WaitQueue(clock: .continuous)
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
      // Normally will run only once. Use `>=` so a sum of exactly 1e9 normalizes
      // to (seconds+1, 0) rather than leaving a denormalized (seconds, 1e9).
      while nanoseconds >= 1_000_000_000 {
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

#if $Embedded
  // The two methods below deliberately mirror the names of the runtime entry
  // points they back (`swift_task_enqueueGlobalWith{Delay,Deadline}`); see
  // ExecutorImpl.swift. They are the Embedded scheduling path, which has no
  // `Clock` value to dispatch on — the generic `enqueue(_:after:tolerance:clock:)`
  // SchedulingExecutor conformance below covers the non-Embedded path.

  /// Enqueue `job` after a raw nanosecond delay, measured on the suspending clock.
  func enqueueGlobalWithDelay(_ job: consuming ExecutorJob, nanoseconds: UInt64) {
    let delay = Duration(seconds: Int64(nanoseconds / 1_000_000_000),
                         nanoseconds: Int64(nanoseconds % 1_000_000_000))
    suspendingWaitQueue.enqueue(job, after: delay)
  }

  /// Enqueue `job` at a deadline given as clock-absolute `(seconds, nanoseconds)`
  /// on `clock`. The deadline is converted to a delay relative to that clock's
  /// current time and enqueued on the matching wait queue. This is equivalent in
  /// observed wake time to `swift_task_enqueueGlobalWithDeadlineImpl` in
  /// CooperativeGlobalExecutor.cpp, which instead re-anchors every clock onto a
  /// single suspending timebase.
  func enqueueGlobalWithDeadline(_ job: consuming ExecutorJob,
                                 seconds: Int64, nanoseconds: Int64,
                                 clock: _ClockID) {
    var nowSeconds: Int64 = 0
    var nowNanoseconds: Int64 = 0
    unsafe _getTime(seconds: &nowSeconds, nanoseconds: &nowNanoseconds,
                    clock: clock.rawValue)
    var deltaSeconds = seconds - nowSeconds
    var deltaNanoseconds = nanoseconds - nowNanoseconds
    if deltaNanoseconds < 0 {
      deltaSeconds -= 1
      deltaNanoseconds += 1_000_000_000
    }
    if deltaSeconds < 0 {
      deltaSeconds = 0
      deltaNanoseconds = 0
    }
    let delay = Duration(seconds: deltaSeconds, nanoseconds: deltaNanoseconds)
    if clock == .continuous {
      continuousWaitQueue.enqueue(job, after: delay)
    } else {
      suspendingWaitQueue.enqueue(job, after: delay)
    }
  }
#endif

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

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    // If it's a clock we know, get the duration to wait
    if let _ = clock as? ContinuousClock {
      let continuousDuration = delay as! ContinuousClock.Duration
      let duration = Duration(from: continuousDuration)
      continuousWaitQueue.enqueue(job, after: duration)
    } else if let _ = clock as? SuspendingClock {
      let suspendingDuration = delay as! SuspendingClock.Duration
      let duration = Duration(from: suspendingDuration)
      suspendingWaitQueue.enqueue(job, after: duration)
    } else {
      fatalError("Sorry, cannot schedule on an unknown clock")
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
      // Process the timer queues
      suspendingWaitQueue.forEachReadyJob {
        runQueue.push(UnownedJob($0))
      }
      continuousWaitQueue.forEachReadyJob {
        runQueue.push(UnownedJob($0))
      }

      // Now run any queued jobs
      var runQ = runQueue.take()
      while let job = runQ.pop() {
        unsafe ExecutorJob(job).runSynchronously(
          on: self.asUnownedSerialExecutor()
        )
      }

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
