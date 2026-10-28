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
@available(StdlibDeploymentTarget 6.5, *)
struct WaitQueue {
  /// A job waiting for its deadline.
  ///
  /// We keep the deadline and a slot alongside the job, rather than in the
  /// job's executor private data, so that a cancelled job can be moved to the
  /// run queue exactly as we received it, and so that a stale cancellation can never
  /// match a different entry that happens to reuse the same address.
  struct Entry {
    var job: UnownedJob
    var deadline: CooperativeExecutor.Timestamp
    var slot: Int
  }

  /// Where each waiting job is in the heap, so that cancelling one doesn't
  /// have to look for it.
  ///
  /// A job's ID (the one in its token) is the number of its slot and the
  /// generation of that slot, so a token for a job that has already run or
  /// been cancelled can't match another job that reuses the slot.  This is a
  /// class so that the queue's `didPlace` hook can update it while the queue
  /// is being modified.
  final class Slots {
    struct Slot {
      /// For a slot in use, the index in the heap of its entry (never
      /// negative).  For a free slot, the next free slot, as `-2 - next`
      /// (so the end of the list, -1, is -1), which is always negative.
      var index: Int
      var generation: UInt32
    }

    var slots: [Slot] = []

    /// The first free slot, or -1.
    var firstFree = -1

    func allocate() -> Int {
      if firstFree >= 0 {
        let slot = firstFree
        firstFree = -2 - slots[slot].index
        slots[slot].index = 0   // The queue sets this when it places the entry
        return slot
      }
      slots.append(Slot(index: 0, generation: 0))
      return slots.count - 1
    }

    func release(_ slot: Int) {
      slots[slot].index = -2 - firstFree
      slots[slot].generation = (slots[slot].generation + 1) & 0x7fff_ffff
      firstFree = slot
    }

    func setHeapIndex(_ index: Int, of slot: Int) {
      slots[slot].index = index
    }

    func id(of slot: Int) -> UInt64 {
      return (UInt64(slots[slot].generation) << 32) | UInt64(slot)
    }

    /// The heap index for a job ID, or nil if it is no longer waiting.
    func heapIndex(forID id: UInt64) -> (slot: Int, index: Int)? {
      let slot = Int(truncatingIfNeeded: id & 0xffff_ffff)
      if slot >= slots.count
           || slots[slot].generation != UInt32(truncatingIfNeeded: id >> 32)
           || slots[slot].index < 0 {
        return nil
      }
      return (slot: slot, index: slots[slot].index)
    }
  }

  var queue: PriorityQueue<Entry>
  let slots = Slots()
  var clock: _ClockID

  init(clock: _ClockID) {
    let slots = self.slots
    queue = PriorityQueue(
      compare: { $0.deadline < $1.deadline },
      didPlace: { slots.setHeapIndex($1, of: $0.slot) })
    self.clock = clock
  }

  var currentTime: CooperativeExecutor.Timestamp {
    var now: CooperativeExecutor.Timestamp = .zero
    unsafe _getTime(seconds: &now.seconds,
                    nanoseconds: &now.nanoseconds,
                    clock: clock.rawValue)
    return now
  }

  /// Returns the ID of the waiting job.
  mutating func enqueue(_ job: consuming ExecutorJob,
                        after delay: CooperativeExecutor.Duration) -> UInt64 {
    let deadline = currentTime + delay
    let slot = slots.allocate()
    queue.push(Entry(job: UnownedJob(job), deadline: deadline, slot: slot))
    return slots.id(of: slot)
  }

  mutating func forEachReadyJob(body: (consuming ExecutorJob) -> ()) {
    let now = currentTime
    while let entry = queue.pop(when: { $0.deadline < now }) {
      slots.release(entry.slot)
      body(ExecutorJob(entry.job))
    }
  }

  /// Remove the job with the specified ID, if it is still waiting.
  mutating func cancel(id: UInt64) -> ExecutorJob? {
    guard let (slot, index) = slots.heapIndex(forID: id) else {
      return nil
    }
    let entry = queue.remove(at: index)
    slots.release(slot)
    return ExecutorJob(entry.job)
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
/// It runs everything on whichever thread calls `run()` or `runUntil(_:)`.
///
/// This executor is intended for single-threaded use (it is the default
/// executor on platforms without threads), so it deliberately has no locking:
/// its run queue and wait queues are not safe to touch from more than one
/// thread.  That means that every operation must be performed from the thread
/// that is running it.  In particular, jobs must be enqueued from that thread,
/// and so must cancellation of a timer, even though `JobCanceller` allows
/// `cancel` to be called from any thread: cancelling a `Task.sleep()` from some
/// other thread while the executor is running races with the executor, and is
/// not supported.  Code that needs to cancel from another thread should use an
/// executor that is built to be used from several.
@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.5, *)
public final class CooperativeExecutor: Executor, @unchecked Sendable {
  var runQueue: PriorityQueue<UnownedJob>
  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  var suspendingWaitQueue = WaitQueue(clock: .suspending)
  var continuousWaitQueue = WaitQueue(clock: .continuous)
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
@available(StdlibDeploymentTarget 6.5, *)
extension CooperativeExecutor: SchedulingExecutor {

  func currentTime(clock: _ClockID) -> Timestamp {
    var now: Timestamp = .zero
    unsafe _getTime(seconds: &now.seconds,
                    nanoseconds: &now.nanoseconds,
                    clock: clock.rawValue)
    return now
  }

  // The clock that a job was scheduled on, which we encode in the token
  private static var continuousClockToken: Int { 1 }
  private static var suspendingClockToken: Int { 2 }

  /// Encode a job ID and its clock in a token's opaque data.
  ///
  /// A token has two words, which on a 32-bit platform is only enough for 64
  /// bits in all, so we put the clock in the low bit of the ID and split the
  /// result into two halves, which works wherever `Int` is at least 32 bits.
  private static func tokenData(id: UInt64, clockToken: Int)
    -> InlineArray<2, Int> {
    let value = (id << 1) | (clockToken == suspendingClockToken ? 1 : 0)
    return [Int(truncatingIfNeeded: value & 0xffff_ffff),
            Int(truncatingIfNeeded: value >> 32)]
  }

  /// The inverse of `tokenData(id:clockToken:)`.
  private static func decode(_ token: borrowing JobCancellationToken)
    -> (id: UInt64, clockToken: Int) {
    let value = UInt64(UInt32(truncatingIfNeeded: token.opaqueData[0]))
      | (UInt64(UInt32(truncatingIfNeeded: token.opaqueData[1])) << 32)
    return (id: value >> 1,
            clockToken: (value & 1) != 0 ? suspendingClockToken
                                         : continuousClockToken)
  }

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
  public func schedule<C: Clock>(_ job: consuming ExecutorJob,
                                 at fireTime: SchedulingExecutorFireTime<C>,
                                 tolerance: C.Duration?,
                                 clock: C) -> JobCancellationToken {
    // A clock we don't recognise may know how to schedule the job itself.
    if !isKnown(clock) && _isSchedulingClock(clock) {
      return _scheduleUsingClock(job, on: self, at: fireTime,
                                tolerance: tolerance, clock: clock)
    }

    let (delay, clockToken) = delay(run: fireTime, clock: clock)
    let id: UInt64
    if clockToken == Self.continuousClockToken {
      id = continuousWaitQueue.enqueue(job, after: delay)
    } else {
      id = suspendingWaitQueue.enqueue(job, after: delay)
    }
    return unsafe JobCancellationToken(
      canceller: self,
      opaqueData: Self.tokenData(id: id, clockToken: clockToken))
  }

  public func supportsScheduling(on clock: SystemClockID) -> Bool {
    return true
  }

  public func cancel(jobWithToken token: borrowing JobCancellationToken)
    -> Bool {
    let (id, clockToken) = Self.decode(token)
    let job: ExecutorJob?
    switch clockToken {
    case Self.continuousClockToken:
      job = continuousWaitQueue.cancel(id: id)
    case Self.suspendingClockToken:
      job = suspendingWaitQueue.cancel(id: id)
    default:
      job = nil
    }
    // Run the job now, rather than when its timer fires.
    switch consume job {
    case .some(let job):
      runOrEnqueue(job)
      return true
    case .none:
      return false
    }
  }

  /// Run a job that has become due, or been cancelled: at once, if all it does
  /// is resume a task (so that the task is enqueued at its own priority, which
  /// may be higher than the job's), and otherwise by enqueuing it.
  func runOrEnqueue(_ job: consuming ExecutorJob) {
    if job.isResumption {
      unsafe job.runSynchronously(on: self.asUnownedSerialExecutor())
    } else {
      enqueue(job)
    }
  }

}
#endif

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.5, *)
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
        runOrEnqueue($0)
      }
      continuousWaitQueue.forEachReadyJob {
        runOrEnqueue($0)
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
@available(StdlibDeploymentTarget 6.5, *)
extension CooperativeExecutor: SerialExecutor {}

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.5, *)
extension CooperativeExecutor: TaskExecutor {}

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.5, *)
extension CooperativeExecutor: MainExecutor {}

#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
