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
  /// We keep the deadline alongside the job, rather than in the job's executor
  /// private data, so that a cancelled job can be moved to the run queue
  /// exactly as we received it.
  ///
  /// Where it is in the heap, so that cancelling it doesn't have to look for
  /// it, is kept in one of two places, which `handle` says.  A job that the
  /// runtime scheduled itself (a sleeping task's continuation) has storage for
  /// us, which is where we keep that: `handle` is the address of that storage,
  /// which we fetch once, when the job is scheduled, since every swap in the
  /// heap has to update it.  For any other job we use a slot (see `Slots`),
  /// and `handle` is the number of the slot, shifted left and with the low bit
  /// set (an address is never odd).
  struct Entry {
    var job: UnownedJob
    var deadline: CooperativeExecutor.Timestamp
    var handle: Int

    init(job: UnownedJob, deadline: CooperativeExecutor.Timestamp, slot: Int) {
      self.job = job
      self.deadline = deadline
      self.handle = (slot << 1) | 1
    }

    init(job: UnownedJob, deadline: CooperativeExecutor.Timestamp,
         storage: UnsafeMutableRawPointer) {
      self.job = job
      self.deadline = deadline
      self.handle = Int(bitPattern: storage)
    }

    /// The slot that keeps this entry's place in the heap, if it uses one.
    var slot: Int? {
      return (handle & 1) != 0 ? handle >> 1 : nil
    }

    /// The storage that keeps this entry's place in the heap, if it uses it.
    var storage: UnsafeMutableRawPointer? {
      return (handle & 1) == 0 ? unsafe UnsafeMutableRawPointer(bitPattern: handle)
                               : nil
    }
  }

  /// What we keep in a job's own storage, which is the first two of its words:
  /// where the job is in the heap, plus one so that the zero that the storage
  /// starts out as means that it isn't (and is again once the job is taken
  /// off the heap), and which of our wait queues the job is in.
  private static func heapIndex(of job: UnownedJob) -> Int? {
    let stored: Int = job.withUnsafeScheduledPrivateData { storage in
      unsafe storage?.load(as: Int.self) ?? 0
    }
    return stored > 0 ? stored - 1 : nil
  }

  private static func setHeapIndex(_ index: Int?,
                                   in storage: UnsafeMutableRawPointer) {
    unsafe storage.storeBytes(of: index.map { $0 + 1 } ?? 0, as: Int.self)
  }

  /// Which of the executor's clocks a job in the heap is waiting on (see
  /// `CooperativeExecutor.continuousClockToken`), kept next to its heap index.
  static func clockToken(of job: UnownedJob) -> Int {
    return job.withUnsafeScheduledPrivateData { storage in
      unsafe storage?.load(fromByteOffset: MemoryLayout<Int>.stride,
                           as: Int.self) ?? 0
    }
  }

  static func setClockToken(_ token: Int, in storage: UnsafeMutableRawPointer) {
    unsafe storage.storeBytes(of: token,
                              toByteOffset: MemoryLayout<Int>.stride,
                              as: Int.self)
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
      didPlace: {
        if let slot = $0.slot {
          slots.setHeapIndex($1, of: slot)
        } else {
          unsafe Self.setHeapIndex($1, in: $0.storage!)
        }
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

  /// Returns the ID of the waiting job.
  mutating func enqueue(_ job: consuming ExecutorJob,
                        after delay: CooperativeExecutor.Duration) -> UInt64 {
    let deadline = currentTime + delay
    let slot = slots.allocate()
    queue.push(Entry(job: UnownedJob(job), deadline: deadline, slot: slot))
    return slots.id(of: slot)
  }

  /// Wait for a job that has storage of its own, which keeps where it is in
  /// the heap, so that there is no ID and no slot.  The job is the token.
  mutating func enqueueScheduled(_ job: UnownedJob,
                                 storage: UnsafeMutableRawPointer,
                                 after delay: CooperativeExecutor.Duration) {
    let deadline = currentTime + delay
    unsafe queue.push(Entry(job: job, deadline: deadline, storage: storage))
  }

  mutating func forEachReadyJob(body: (consuming ExecutorJob) -> ()) {
    let now = currentTime
    while let entry = queue.pop(when: { $0.deadline < now }) {
      if let slot = entry.slot {
        slots.release(slot)
      } else {
        unsafe Self.setHeapIndex(nil, in: entry.storage!)
      }
      body(ExecutorJob(entry.job))
    }
  }

  /// Remove a job that has its own storage, if it is still waiting.
  ///
  /// The job must not have been run, as it will have been freed if it has,
  /// which is true of a job that the runtime scheduled for a `Task.sleep()`
  /// and cancels it from its cancellation handler.
  mutating func cancelScheduled(_ job: UnownedJob) -> ExecutorJob? {
    guard let index = Self.heapIndex(of: job) else {
      return nil
    }
    let entry = queue.remove(at: index)
    unsafe Self.setHeapIndex(nil, in: entry.storage!)
    return ExecutorJob(entry.job)
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

  /// The queue that `runUntil(_:)` takes the jobs to run from.  It is swapped
  /// with `runQueue` rather than replaced by a new queue, so that both keep
  /// their storage and running a job allocates nothing once they have grown.
  var runningQueue: PriorityQueue<UnownedJob>
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
    runningQueue = PriorityQueue(compare: { $0.priority > $1.priority })
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

  /// The second word of a token for a job that has its own storage, in which
  /// the first word is the address of the job.  It can't be the second word
  /// of any other token, whose halves are 32 bits and which has a generation
  /// of at most 0x7fff_ffff in the high one, shifted left by one.
  private static var addressTokenMarker: Int {
    return Int(truncatingIfNeeded: UInt32.max)
  }

  /// A token for a job that has its own storage, which it refers to by address.
  private static func tokenData(address job: UnownedJob) -> InlineArray<2, Int> {
    return [unsafe unsafeBitCast(job, to: Int.self), addressTokenMarker]
  }

  /// The job that a token refers to by address, or nil if it isn't one.
  private static func jobAddressed(by token: borrowing JobCancellationToken)
    -> UnownedJob? {
    guard UInt32(truncatingIfNeeded: token.opaqueData[1])
            == UInt32(truncatingIfNeeded: addressTokenMarker) else {
      return nil
    }
    return unsafe unsafeBitCast(token.opaqueData[0], to: UnownedJob.self)
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

    // A job that the runtime scheduled has storage of its own, which is
    // where we keep our bookkeeping for it, so that we don't need a slot or an
    // ID, and the token is just the job.
    if let storage = job.withUnsafeScheduledPrivateData(
         body: { unsafe $0?.baseAddress }) {
      unsafe WaitQueue.setClockToken(clockToken, in: storage)
      let scheduled = UnownedJob(job)
      if clockToken == Self.continuousClockToken {
        unsafe continuousWaitQueue.enqueueScheduled(scheduled, storage: storage,
                                                    after: delay)
      } else {
        unsafe suspendingWaitQueue.enqueueScheduled(scheduled, storage: storage,
                                                    after: delay)
      }
      return unsafe JobCancellationToken(
        canceller: self, opaqueData: Self.tokenData(address: scheduled))
    }

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
    let job: ExecutorJob?
    if let scheduled = Self.jobAddressed(by: token) {
      // A job with storage of its own, which the runtime only cancels while it
      // is still waiting (and so while it is still there to look at).
      switch WaitQueue.clockToken(of: scheduled) {
      case Self.continuousClockToken:
        job = continuousWaitQueue.cancelScheduled(scheduled)
      case Self.suspendingClockToken:
        job = suspendingWaitQueue.cancelScheduled(scheduled)
      default:
        job = nil
      }
    } else {
      let (id, clockToken) = Self.decode(token)
      switch clockToken {
      case Self.continuousClockToken:
        job = continuousWaitQueue.cancel(id: id)
      case Self.suspendingClockToken:
        job = suspendingWaitQueue.cancel(id: id)
      default:
        job = nil
      }
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
      // Jobs that are enqueued while we do that go in `runQueue`, which is
      // empty (and keeps the storage that the queue that we have just drained
      // had) after the swap.
      swap(&runQueue, &runningQueue)
      while let job = runningQueue.pop() {
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
