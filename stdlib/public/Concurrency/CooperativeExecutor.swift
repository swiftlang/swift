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

  /// What we keep in a job's own storage, a word each:
  ///
  /// * the job's `JobState`, which includes where it is in the heap;
  /// * which of our wait queues the job is in (see
  ///   `CooperativeExecutor.continuousClockToken`).
  ///
  /// We can look at the storage of a job when we are asked to cancel it,
  /// because the runtime only asks while the job is there (see the
  /// documentation of `JobCanceller.cancel(jobWithToken:)`), and so the token
  /// for such a job is just its address.
  private enum Word {
    static let heapIndex = 0
    static let clockToken = 1
  }

  /// What a job with storage of its own is doing, as far as we are concerned,
  /// which is kept in the first word of its storage.
  ///
  /// There is nothing for a job that has been run: we are not asked about those,
  /// since the runtime only cancels a job that has not run, and running one
  /// can free its storage.
  enum JobState: Equatable {
    /// We have not got it, which is what the zero that its storage starts out
    /// as means.
    case notQueued

    /// It is in the heap, at this index.
    case queued(heapIndex: Int)

    /// Its timer fired, but it had been cancelled first, so `cancel` has to run
    /// it.
    case cancelOwed

    init(rawValue: Int) {
      switch rawValue {
      case 0: self = .notQueued
      case -1: self = .cancelOwed
      case 1...: self = .queued(heapIndex: rawValue - 1)
      default:
        fatalError("a job's storage has a state that the cooperative executor "
                   + "didn't put in it: \(rawValue)")
      }
    }

    var rawValue: Int {
      switch self {
      case .notQueued: return 0
      case .queued(let heapIndex): return heapIndex + 1
      case .cancelOwed: return -1
      }
    }
  }

  private static func load(_ word: Int,
                           from storage: UnsafeMutableRawPointer) -> Int {
    return unsafe storage.load(fromByteOffset: word * MemoryLayout<Int>.stride,
                               as: Int.self)
  }

  private static func store(_ value: Int, _ word: Int,
                            in storage: UnsafeMutableRawPointer) {
    unsafe storage.storeBytes(of: value,
                              toByteOffset: word * MemoryLayout<Int>.stride,
                              as: Int.self)
  }

  static func state(in storage: UnsafeMutableRawPointer) -> JobState {
    return JobState(rawValue: unsafe load(Word.heapIndex, from: storage))
  }

  static func setState(_ state: JobState,
                       in storage: UnsafeMutableRawPointer) {
    unsafe store(state.rawValue, Word.heapIndex, in: storage)
  }

  static func clockToken(in storage: UnsafeMutableRawPointer) -> Int {
    return unsafe load(Word.clockToken, from: storage)
  }

  /// Set a job's storage up, when we are given it to schedule.  It is not
  /// necessarily as the runtime made it: the job may have been through another
  /// scheduler, or a clock, which has used it.
  static func prepare(_ storage: UnsafeMutableRawPointer, clockToken: Int) {
    unsafe setState(.notQueued, in: storage)
    unsafe store(clockToken, Word.clockToken, in: storage)
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
          unsafe Self.setState(.queued(heapIndex: $1), in: $0.storage!)
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
        body(ExecutorJob(entry.job))
        continue
      }

      // The runtime decides between the timer and a cancellation for a job
      // like this one, so we have to ask it whether the job is ours.  If it
      // isn't, it was cancelled first, and `cancel` has been, or is about to
      // be, called, and runs it.  That can't happen here, since we run
      // everything on one thread, and cancel from it, so this costs a call to
      // the runtime for each timer that fires for nothing that we need; it is
      // here because this is how a scheduler is meant to do it, and this
      // executor is the one that others will read.
      let storage = unsafe entry.storage!
      if entry.job.claimForTimer() {
        // It is no longer in the heap.  (Do this before running it, which can
        // free its storage.)
        unsafe Self.setState(.notQueued, in: storage)
        body(ExecutorJob(entry.job))
      } else {
        unsafe Self.setState(.cancelOwed, in: storage)
      }
    }
  }

  /// Cancel a job that has its own storage, which is `storage`.  Returns the job
  /// if we are to run it: it is still waiting, so we take it off the heap, or
  /// its timer fired but it was cancelled first and we left it for this.
  ///
  /// The job has to be there to be cancelled: the runtime only asks for a job
  /// that has not run.  And since it has decided that the job is cancelled, and
  /// won't let its timer have it, nothing but this will run it, so it is an error
  /// for it to be neither in the heap, where `didPlace` said it was, nor owed to
  /// us; rather than lose the job, and leave its task waiting forever, we stop.
  mutating func cancelScheduled(_ job: UnownedJob,
                                storage: UnsafeMutableRawPointer)
    -> ExecutorJob {
    switch unsafe Self.state(in: storage) {
    case .queued(let index):
      guard index < queue.storage.count
              && Self.same(queue.storage[index].job, job) else {
        fatalError("the cooperative executor was asked to cancel a job that "
                   + "isn't where it says it is in the heap")
      }
      _ = queue.remove(at: index)
    case .cancelOwed:
      break
    case .notQueued:
      // `schedule` puts the job in the heap before it returns its token, so
      // we can only get here if we are asked about a job that has run.
      fatalError("the cooperative executor was asked to cancel a job that it "
                 + "doesn't have")
    }
    unsafe Self.setState(.notQueued, in: storage)
    return ExecutorJob(job)
  }

  /// Whether two jobs are the same one.
  private static func same(_ a: UnownedJob, _ b: UnownedJob) -> Bool {
    return unsafe unsafeBitCast(a, to: Int.self) == unsafeBitCast(b, to: Int.self)
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

  /// A queue that `runUntil(_:)` has finished with, which still has the
  /// storage that it grew, kept so that the next trip round the loop can use it
  /// instead of allocating a new one.  It is `nil` while a loop has it, so a
  /// `runUntil(_:)` that is called from a job that the first is running gets a
  /// queue of its own.
  var spareRunQueue: PriorityQueue<UnownedJob>?
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

  /// A new, empty run queue, which runs higher priority jobs first.
  private static func makeRunQueue() -> PriorityQueue<UnownedJob> {
    return PriorityQueue(compare: { $0.priority > $1.priority })
  }

  public init() {
    runQueue = Self.makeRunQueue()
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

  /// A token for a job that has its own storage, which it refers to by
  /// address.  That is enough to identify it, since the runtime only cancels a
  /// job of this kind that has not run (see `JobCanceller.cancel`).
  private static func tokenData(address job: UnownedJob) -> InlineArray<2, Int> {
    return [unsafe unsafeBitCast(job, to: Int.self), addressTokenMarker]
  }

  /// The job that a token refers to by address, or nil if it isn't that kind of
  /// token.
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
      unsafe WaitQueue.prepare(storage, clockToken: clockToken)
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
    -> JobCancellationResult {
    let job: ExecutorJob?
    if let scheduled = Self.jobAddressed(by: token) {
      // A job with storage of its own, which the runtime only cancels while it
      // is still there, so we can look at it.  (It asks at most once, too.)
      guard let storage = scheduled.withUnsafeScheduledPrivateData(
              body: { unsafe $0?.baseAddress }) else {
        return .cancelled
      }
      switch unsafe WaitQueue.clockToken(in: storage) {
      case Self.continuousClockToken:
        job = unsafe continuousWaitQueue.cancelScheduled(scheduled,
                                                         storage: storage)
      case Self.suspendingClockToken:
        job = unsafe suspendingWaitQueue.cancelScheduled(scheduled,
                                                         storage: storage)
      default:
        fatalError("a job's storage has a clock that the cooperative executor "
                   + "didn't put in it")
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
    // We are on the only thread that touches the timers, so we can always give
    // the job back, and let whoever is cancelling it run it now, rather than
    // when its timer fires.
    switch consume job {
    case .some(let job):
      return .removed(job)
    case .none:
      return .cancelled
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

  /// Run jobs until `condition` is true, or there are no more to run.
  ///
  /// This can be called from a job that the executor is running.  The jobs that
  /// the outer call has taken from the queue, but not yet run, wait until the
  /// inner call returns: it runs those that are enqueued, and timers that
  /// expire, from then on, so a condition that depends on one of the others
  /// will not become true while it runs.
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
      // Take the queue, leaving an empty one, so that jobs that are enqueued
      // while we run these (by them, or by a `runUntil(_:)` that one of them
      // calls) are not mixed up with them.  The empty one is last time's, so
      // that no storage has to be allocated for it.
      var runQ = spareRunQueue ?? Self.makeRunQueue()
      spareRunQueue = nil
      swap(&runQueue, &runQ)
      while let job = runQ.pop() {
        unsafe ExecutorJob(job).runSynchronously(
          on: self.asUnownedSerialExecutor()
        )
      }
      spareRunQueue = runQ

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
