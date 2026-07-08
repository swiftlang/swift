//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if !$Embedded && !os(WASI) && !os(Emscripten)

import Swift

// We can't import Dispatch from here, sadly, because it apparently has a
// transitive dependency on Combine (which in turn depends on _Concurrency).

// import Dispatch

// .. Dispatch Interface .......................................................

// .. Main Executor ............................................................

/// A Dispatch-based main executor.
@available(StdlibDeploymentTarget 9999, *)
class DispatchMainExecutor: RunLoopExecutor,
                            ContinuousClockExecutor, SuspendingClockExecutor,
                            @unchecked Sendable {
  var threaded = false
  let _timerScheduler: DispatchTimerScheduler

  public init() {
    self._timerScheduler = DispatchTimerScheduler(runJob: { _dispatchEnqueueMain($0) }, usesMainQueue: true)
  }

  public func run() throws {
    if threaded {
      fatalError("DispatchMainExecutor does not support recursion")
    }

    threaded = true
    _dispatchMain()
  }

  public func stop() {
    fatalError("DispatchMainExecutor cannot be stopped")
  }

  public func cancel(_ token: OperationExecutorRegistration) {
    self._timerScheduler.cancel(token)
  }

  public func escalatePriority(
    of token: OperationExecutorRegistration,
    to newPriority: TaskPriority
  ) {
    self._timerScheduler.escalatePriority(token, to: newPriority)
  }

  public func enqueue(
    _ job: consuming ExecutorJob,
    at instant: ContinuousClock.Instant,
    tolerance: ContinuousClock.Duration?
  ) -> OperationExecutorRegistration {
    let timestamp = ContinuousClock().timestamp(for: instant)
    let deadline = DispatchTimerScheduler.Deadline(
      seconds: timestamp.seconds,
      nanoseconds: timestamp.nanoseconds
    )

    let priority = job.priority.rawValue
    return self._timerScheduler.arm(
      work: .job(UnownedJob(job)),
      deadline: deadline,
      clockID: timestamp.clockID.rawValue,
      priority: priority
    )
  }

  public func enqueue(
    _ job: consuming ExecutorJob,
    at instant: SuspendingClock.Instant,
    tolerance: SuspendingClock.Duration?
  ) -> OperationExecutorRegistration {
    let timestamp = SuspendingClock().timestamp(for: instant)
    let deadline = DispatchTimerScheduler.Deadline(
      seconds: timestamp.seconds,
      nanoseconds: timestamp.nanoseconds
    )

    let priority = job.priority.rawValue
    return self._timerScheduler.arm(
      work: .job(UnownedJob(job)),
      deadline: deadline,
      clockID: timestamp.clockID.rawValue,
      priority: priority
    )
  }

  public func enqueue(
    _ continuation: consuming ExecutorContinuation<Void, CancellationError>,
    at instant: ContinuousClock.Instant,
    tolerance: ContinuousClock.Duration?
  ) -> OperationExecutorRegistration {
    let timestamp = ContinuousClock().timestamp(for: instant)
    let deadline = DispatchTimerScheduler.Deadline(
      seconds: timestamp.seconds,
      nanoseconds: timestamp.nanoseconds
    )

    return self._timerScheduler.arm(
      continuation,
      deadline: deadline,
      clockID: timestamp.clockID.rawValue,
      priority: Task.currentPriority.rawValue
    )
  }

  public func enqueue(
    _ continuation: consuming ExecutorContinuation<Void, CancellationError>,
    at instant: SuspendingClock.Instant,
    tolerance: SuspendingClock.Duration?
  ) -> OperationExecutorRegistration {
    let timestamp = SuspendingClock().timestamp(for: instant)
    let deadline = DispatchTimerScheduler.Deadline(
      seconds: timestamp.seconds,
      nanoseconds: timestamp.nanoseconds
    )

    return self._timerScheduler.arm(
      continuation,
      deadline: deadline,
      clockID: timestamp.clockID.rawValue,
      priority: Task.currentPriority.rawValue
    )
  }
}

@available(StdlibDeploymentTarget 9999, *)
extension DispatchMainExecutor: SerialExecutor {

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueMain(UnownedJob(job))
  }

  public func checkIsolated() {
    _dispatchAssertMainQueue()
  }
}

@available(StdlibDeploymentTarget 9999, *)
extension DispatchMainExecutor: MainExecutor {}

// .. Task Executor ............................................................

/// A Dispatch-based `TaskExecutor`
@available(StdlibDeploymentTarget 9999, *)
class DispatchGlobalTaskExecutor: TaskExecutor,
                                  ContinuousClockExecutor, SuspendingClockExecutor,
                                  @unchecked Sendable {
  let _timerScheduler: DispatchTimerScheduler

  public init() {
    self._timerScheduler = DispatchTimerScheduler(runJob: { _dispatchEnqueueGlobal($0) }, usesMainQueue: false)
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueGlobal(UnownedJob(job))
  }

  public func enqueue(
    _ job: consuming ExecutorJob,
    at instant: ContinuousClock.Instant,
    tolerance: ContinuousClock.Duration?
  ) -> OperationExecutorRegistration {
    let timestamp = ContinuousClock().timestamp(for: instant)
    let deadline = DispatchTimerScheduler.Deadline(
      seconds: timestamp.seconds,
      nanoseconds: timestamp.nanoseconds
    )

    let priority = job.priority.rawValue
    return self._timerScheduler.arm(
      work: .job(UnownedJob(job)),
      deadline: deadline,
      clockID: timestamp.clockID.rawValue,
      priority: priority
    )
  }

  public func enqueue(
    _ job: consuming ExecutorJob,
    at instant: SuspendingClock.Instant,
    tolerance: SuspendingClock.Duration?
  ) -> OperationExecutorRegistration {
    let timestamp = SuspendingClock().timestamp(for: instant)
    let deadline = DispatchTimerScheduler.Deadline(
      seconds: timestamp.seconds,
      nanoseconds: timestamp.nanoseconds
    )

    let priority = job.priority.rawValue
    return self._timerScheduler.arm(
      work: .job(UnownedJob(job)),
      deadline: deadline,
      clockID: timestamp.clockID.rawValue,
      priority: priority
    )
  }

  public func enqueue(
    _ continuation: consuming ExecutorContinuation<Void, CancellationError>,
    at instant: ContinuousClock.Instant,
    tolerance: ContinuousClock.Duration?
  ) -> OperationExecutorRegistration {
    let timestamp = ContinuousClock().timestamp(for: instant)
    let deadline = DispatchTimerScheduler.Deadline(
      seconds: timestamp.seconds,
      nanoseconds: timestamp.nanoseconds
    )

    return self._timerScheduler.arm(
      continuation,
      deadline: deadline,
      clockID: timestamp.clockID.rawValue,
      priority: Task.currentPriority.rawValue
    )
  }

  public func enqueue(
    _ continuation: consuming ExecutorContinuation<Void, CancellationError>,
    at instant: SuspendingClock.Instant,
    tolerance: SuspendingClock.Duration?
  ) -> OperationExecutorRegistration {
    let timestamp = SuspendingClock().timestamp(for: instant)
    let deadline = DispatchTimerScheduler.Deadline(
      seconds: timestamp.seconds,
      nanoseconds: timestamp.nanoseconds
    )

    return self._timerScheduler.arm(
      continuation,
      deadline: deadline,
      clockID: timestamp.clockID.rawValue,
      priority: Task.currentPriority.rawValue
    )
  }

  public func cancel(_ token: OperationExecutorRegistration) {
    self._timerScheduler.cancel(token)
  }

  public func escalatePriority(
    of token: OperationExecutorRegistration,
    to newPriority: TaskPriority
  ) {
    self._timerScheduler.escalatePriority(token, to: newPriority)
  }
}

// .. Clock Support ............................................................

@_unavailableInEmbedded
extension ContinuousClock {
  func timestamp(for instant: Instant)
    -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, nanoseconds) = durationComponents(for: instant._value)
    return (clockID: .continuous, seconds: seconds, nanoseconds: nanoseconds)
  }

  func durationComponents(for duration: Duration)
    -> (seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000
    return (seconds: seconds, nanoseconds: nanoseconds)
  }
}

@_unavailableInEmbedded
extension SuspendingClock {
  func timestamp(for instant: Instant)
    -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, nanoseconds) = durationComponents(for: instant._value)
    return (clockID: .suspending, seconds: seconds, nanoseconds: nanoseconds)
  }

  func durationComponents(for duration: Duration)
    -> (seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000
    return (seconds: seconds, nanoseconds: nanoseconds)
  }
}


// .. Concurrency primitives ..................................................
//
// `_Concurrency` cannot depend on the `Synchronization` module: `Synchronization`
// links the platform overlay (`swiftDarwin`, Glibc, ...), which sits *above*
// `_Concurrency` in the runtime layering, so importing it would invert the
// dependency.  The scheduler therefore uses these two local primitives instead
// of `Synchronization.Mutex` / `Atomic`:
//
//  * `Mutex` wraps the C++ `swift::Mutex` from the (Core) Threading library
//    through the `_swift_async_stream_lock_*` shims -- reusing the module-internal
//    `_lock`/`_unlock`/`_lockInit`/`_lockWordCount` helpers declared in
//    `AsyncStreamBuffer.swift`, the same primitive `AsyncStream` uses -- so no
//    overlay is pulled in.
//  * `Atomic` is a lock-free machine word backed directly by a `Builtin` atomic
//    (the idiom `CheckedContinuation` already uses), so it needs no dependency.

/// A mutual-exclusion lock guarding a value of type `State`.
@available(StdlibDeploymentTarget 9999, *)
@safe
final class Mutex<State>: @unchecked Sendable {
  private var _value: State
  /// Storage for the underlying `swift::Mutex`, sized by the shim.  It is never
  /// torn down while in use; the scheduler that owns it lives for the process.
  private let _lockStorage: UnsafeMutableRawPointer

  init(_ initialValue: consuming State) {
    self._value = initialValue
    // `_lockWordCount()` is in units of pointer-width words.
    let byteCount = _lockWordCount() * MemoryLayout<UInt>.stride
    unsafe self._lockStorage = UnsafeMutableRawPointer.allocate(
      byteCount: byteCount,
      alignment: MemoryLayout<UInt>.alignment)
    unsafe _lockInit(UnsafeRawPointer(_lockStorage))
  }

  /// Runs `body` with the lock held, granting it exclusive access to the state.
  func withLock<Result>(_ body: (inout State) -> Result) -> Result {
    unsafe _lock(UnsafeRawPointer(_lockStorage))
    defer { unsafe _unlock(UnsafeRawPointer(_lockStorage)) }
    return body(&_value)
  }

  deinit {
    unsafe _lockStorage.deallocate()
  }
}

/// The memory ordering for an `Atomic` operation.  The scheduler only needs
/// `.relaxed`, which is all a monotonic id source requires.
@available(StdlibDeploymentTarget 9999, *)
@safe
struct AtomicUpdateOrdering {
  static var relaxed: AtomicUpdateOrdering { AtomicUpdateOrdering() }
}

/// A lock-free atomic integer backed by a `Builtin` atomic on a machine word.
@available(StdlibDeploymentTarget 9999, *)
@safe
final class Atomic<Value>: @unchecked Sendable {
  private let _storage: UnsafeMutablePointer<Value>

  init(_ initialValue: Value) {
    unsafe self._storage = UnsafeMutablePointer<Value>.allocate(capacity: 1)
    unsafe _storage.initialize(to: initialValue)
  }

  deinit {
    unsafe _storage.deinitialize(count: 1)
    unsafe _storage.deallocate()
  }
}

@available(StdlibDeploymentTarget 9999, *)
extension Atomic where Value == UInt {
  /// Atomically adds `operand`, returning the values before and after the add.
  func wrappingAdd(
    _ operand: UInt, ordering: AtomicUpdateOrdering
  ) -> (oldValue: UInt, newValue: UInt) {
    let rawPointer = unsafe unsafeBitCast(_storage, to: Builtin.RawPointer.self)
    let oldWord = Builtin.atomicrmw_add_monotonic_Word(
      rawPointer, Int(bitPattern: operand)._builtinWordValue)
    let oldValue = unsafe unsafeBitCast(oldWord, to: UInt.self)
    return (oldValue, oldValue &+ operand)
  }
}


// .. Swift-owned timer scheduler .............................................
//
// All scheduling state for the Dispatch executors' typed timers lives here in
// Swift; the only C interop is a dumb, re-armable per-clock timer, confined to
// `DispatchTimer` below.  Dispatch's own timer manager already serializes a
// source's callbacks (its work-stealing timer queue owns that), so a single
// `Mutex` guarding both clocks' state is sufficient -- no sharding and no
// per-clock locks are needed.  `enqueue` (via `arm`) mints token ids from a
// lock-free `Atomic` counter, so it never needs an upfront registration step.
@available(StdlibDeploymentTarget 9999, *)
@safe
final class DispatchTimerScheduler: @unchecked Sendable {
  /// A point in time on one of the clocks, as whole seconds plus nanoseconds.
  struct Deadline: Comparable {
    var seconds: Int64
    var nanoseconds: Int64
    static func < (lhs: Deadline, rhs: Deadline) -> Bool {
      (lhs.seconds, lhs.nanoseconds) < (rhs.seconds, rhs.nanoseconds)
    }
  }

  /// An entry in a clock's deadline-ordered heap.
  struct HeapEntry {
    var deadline: Deadline
    var token: OperationExecutorRegistration
  }

  /// The work a timer performs when it fires: resume a suspended task's
  /// continuation, or run a scheduled job.
  @safe
  enum Work {
    case continuation(Builtin.RawUnsafeContinuation)
    case job(UnownedJob)
  }

  /// The side effect a state transition asks the caller to perform *outside* the
  /// lock (SBP-008: model actions explicitly and keep side effects out of the
  /// transition -- this avoids re-entrancy while holding the mutex and lets each
  /// transition be reasoned about and tested independently of its effects).
  @safe
  enum Action {
    case none
    case resumeSuccess(Builtin.RawUnsafeContinuation)
    case resumeCancelled(Builtin.RawUnsafeContinuation)
    case run(UnownedJob)
  }

  /// The state of a registered operation in the clock-agnostic map.  Each entry
  /// also remembers the `BucketKey` of the lane it was armed into, so a later
  /// cancellation can find that lane without resuming on the cancelling thread.
  ///
  ///  * `scheduled`: its work is held until the timer fires or it is cancelled.
  ///  * `cancelling`: the operation was cancelled; the continuation must be
  ///    resumed *throwing* `CancellationError`, but on the executor's own timer
  ///    thread rather than the cancelling task's thread.  `cancel` pushes an
  ///    immediate heap entry so the lane's timer fires ASAP and `fire` performs
  ///    the throwing resume (see `cancel`).
  @safe
  enum Pending {
    case scheduled(Work, BucketKey)
    case cancelling(Builtin.RawUnsafeContinuation, BucketKey)
  }

  /// A lazily-created, re-armable Dispatch timer source for a single clock.
  ///
  /// This is the one place that touches the raw C timer interop; the rest of the
  /// scheduler works only in terms of `Deadline`.  The source is created on
  /// first use and re-armed in place -- it is never torn down (the clock timers
  /// live for the process lifetime), so the callback context is retained exactly
  /// once, when the source is created.
  @safe
  struct DispatchTimer {
    private var source: UnsafeMutableRawPointer?

    init() {
      unsafe self.source = nil
    }

    /// Arm the timer to fire no earlier than `deadline`, delivering to `context`.
    mutating func arm(at deadline: Deadline, context: ClockContext) {
      let source = unsafe ensureSource(deliveringTo: context)
      unsafe _dispatchTimerSet(
        source,
        deadline.seconds,
        deadline.nanoseconds,
        0,
        0
      )
    }

    /// Disarm the timer.  A cheap no-op if the source was never created.
    mutating func disarm() {
      if let source = unsafe source {
        unsafe _dispatchTimerDisarm(source)
      }
    }

    /// The source, creating it on first use.
    private mutating func ensureSource(
      deliveringTo context: ClockContext
    ) -> UnsafeMutableRawPointer {
      if let existing = unsafe source {
        return unsafe existing
      }
      // The source keeps a +1 on the context for its (process-long) lifetime;
      // the fire trampoline reads it back with `passUnretained`.
      let callbackContext = unsafe Unmanaged.passRetained(context).toOpaque()
      let created = unsafe _dispatchTimerCreate(
        context.clockID,
        context.qos,
        _dispatchTimerFireTrampoline,
        callbackContext
      )!
      unsafe self.source = created
      return unsafe created
    }
  }

  /// The deadline-ordered heap and dispatch timer for one clock.
  @safe
  struct ClockQueue {
    var heap: PriorityQueue<HeapEntry>
    var timer: DispatchTimer
    let context: ClockContext

    init(context: ClockContext) {
      self.heap = PriorityQueue(compare: { $0.deadline < $1.deadline })
      self.timer = DispatchTimer()
      self.context = context
    }
  }

  /// The key selecting a timer lane: one (clock, qos) pair.
  @safe
  struct BucketKey: Hashable {
    var clockID: Int32
    var qos: Int32
  }

  /// All mutable scheduler state, behind a single `Mutex`.
  @safe
  struct State {
    /// In-flight operations keyed by token; the single-delivery claim point.
    var operations: [OperationExecutorRegistration: Pending]
    /// One deadline heap + dispatch timer per (clock, qos) lane, created lazily.
    var buckets: [BucketKey: ClockQueue]

    init() {
      self.operations = [:]
      self.buckets = [:]
    }
  }

  /// Immutable per-clock context handed to the C timer as its callback context;
  /// the fire trampoline reconstitutes it to route back into the scheduler.
  @available(StdlibDeploymentTarget 9999, *)
  @safe
  final class ClockContext {
    let clockID: Int32
    let qos: Int32
    unowned(unsafe) var scheduler: DispatchTimerScheduler!
    /// Scratch buffer of actions to perform after a firing.  Reused across
    /// firings so `fire` doesn't allocate on the hot path.  Owned exclusively by
    /// this clock's fire handler -- Dispatch serializes a single timer source's
    /// callbacks, so it never runs concurrently with itself and needs no
    /// synchronization of its own.
    var actions: [Action] = []

    init(clockID: Int32, qos: Int32) {
      self.clockID = clockID
      self.qos = qos
      self.actions.reserveCapacity(16)
    }
  }

  let runJob: (UnownedJob) -> Void
  /// When true, all timers deliver on the main queue (the main executor);
  /// otherwise one timer per QoS on the matching global concurrent queue.
  let usesMainQueue: Bool
  /// Lock-free monotonic id source, so `arm` mints a token without extra locking.
  let nextID: Atomic<UInt>
  let state: Mutex<State>

  /// qos sentinel meaning "deliver on the main queue" (used by the main executor).
  static let mainQueueQoS: Int32 = -1

  init(runJob: @escaping (UnownedJob) -> Void, usesMainQueue: Bool) {
    self.runJob = runJob
    self.usesMainQueue = usesMainQueue
    self.nextID = Atomic(0)
    self.state = Mutex(State())
  }

  /// The current time on `clockID`.
  func currentTime(_ clockID: Int32) -> Deadline {
    var seconds: Int64 = 0
    var nanoseconds: Int64 = 0
    unsafe _getTime(seconds: &seconds, nanoseconds: &nanoseconds, clock: clockID)
    return Deadline(seconds: seconds, nanoseconds: nanoseconds)
  }

  // Resume the suspended task with success (its timer fired).
  func resumeSuccess(_ continuation: Builtin.RawUnsafeContinuation) {
    let resumable = ExecutorContinuation<Void, CancellationError>(context: continuation)
    resumable.resumeSynchronously(returning: ())
  }

  // Resume the suspended task by throwing `CancellationError` (it was cancelled).
  func resumeCancelled(_ continuation: Builtin.RawUnsafeContinuation) {
    let resumable = ExecutorContinuation<Void, CancellationError>(context: continuation)
    resumable.resumeSynchronously(throwing: CancellationError())
  }

  // Perform a transition's action.  Always called *outside* `state`'s lock.
  func perform(_ action: Action) {
    switch action {
    case .none: break
    case .resumeSuccess(let continuation): resumeSuccess(continuation)
    case .resumeCancelled(let continuation): resumeCancelled(continuation)
    case .run(let job): runJob(job)
    }
  }

  /// Mint a unique token id; one relaxed atomic per registration, off the lock.
  private func mintToken() -> OperationExecutorRegistration {
    OperationExecutorRegistration(
      id: UInt64(nextID.wrappingAdd(1, ordering: .relaxed).newValue))
  }

  // Re-arm the clock's timer to its earliest pending deadline, or disarm it if
  // nothing is pending.  Must be called with `state` locked.
  func reprogram(_ queue: inout ClockQueue) {
    if let earliest = queue.heap.top {
      queue.timer.arm(at: earliest.deadline, context: queue.context)
    } else {
      queue.timer.disarm()
    }
  }

  // Mint a token, arm a timer for it, and return the token.
  func arm(
    work: Work,
    deadline: Deadline,
    clockID: Int32,
    priority: UInt8
  ) -> OperationExecutorRegistration {
    let token = mintToken()
    let qos = usesMainQueue ? Self.mainQueueQoS : Int32(priority)
    let key = BucketKey(clockID: clockID, qos: qos)
    state.withLock { state in
      var queue: ClockQueue
      if let existing = state.buckets[key] {
        queue = existing
      } else {
        let context = ClockContext(clockID: clockID, qos: qos)
        unsafe context.scheduler = self
        queue = ClockQueue(context: context)
      }
      // Fresh token: never previously registered, so just record it, tagging
      // it with the lane it lives in so a later `cancel` can find it.
      state.operations[token] = .scheduled(work, key)
      queue.heap.push(HeapEntry(deadline: deadline, token: token))
      reprogram(&queue)
      state.buckets[key] = queue
    }
    return token
  }

  // Arm a timer that resumes `continuation` at `deadline`.  Extracts the raw
  // handle up front so it can be stored across the arm/fire boundary.
  func arm(
    _ continuation: consuming ExecutorContinuation<Void, CancellationError>,
    deadline: Deadline,
    clockID: Int32,
    priority: UInt8
  ) -> OperationExecutorRegistration {
    return arm(
      work: .continuation(continuation._takeContext()),
      deadline: deadline,
      clockID: clockID,
      priority: priority
    )
  }

  // Cancellation handler.  Runs on the *cancelling* task's thread (inside a
  // `withTaskCancellationHandler` `onCancel:`), so it must NOT resume the
  // continuation here -- executor continuations may only be resumed from an
  // executor's own run context.  Instead it re-targets the operation to fire
  // immediately on its lane's timer: flip it to `.cancelling` and push a
  // now-deadline heap entry, so the lane's timer wakes ASAP on the executor's
  // thread and `fire` performs the throwing resume.  A scheduled job is simply
  // dropped (nothing to resume).
  func cancel(_ token: OperationExecutorRegistration) {
    state.withLock { state in
      switch state.operations[token] {
      case .scheduled(let work, let key):
        switch work {
        case .continuation(let continuation):
          state.operations[token] = .cancelling(continuation, key)
          if var queue = state.buckets[key] {
            let now = currentTime(key.clockID)
            queue.heap.push(HeapEntry(deadline: now, token: token))
            reprogram(&queue)
            state.buckets[key] = queue
          }
        case .job:
          // Dropping a scheduled job needs no resume.
          state.operations.removeValue(forKey: token)
        }
      case .cancelling, nil:
        // Already resolving, or the op already fired / is unknown: safe miss.
        break
      }
    }
  }

  func escalatePriority(
    _ token: OperationExecutorRegistration,
    to newPriority: TaskPriority
  ) {
    // TODO: With per-QoS timer lanes, an escalation should move this token's
    // heap entry into the lane matching `newPriority` (and reprogram both the
    // old and new lanes' timers) so the resumed continuation is donated at the
    // escalated QoS. That re-bucketing is not implemented yet, so escalation of
    // an in-flight timer is currently a no-op.
  }

  // Pop every entry due at `now` from `queue`, claim each from `operations` (the
  // single-delivery point), and record the action to perform for it.
  private func collectDue(
    from queue: inout ClockQueue,
    operations: inout [OperationExecutorRegistration: Pending],
    now: Deadline,
    into actions: inout [Action]
  ) {
    while let earliest = queue.heap.top, !(now < earliest.deadline) {
      _ = queue.heap.pop()
      switch operations.removeValue(forKey: earliest.token) {
      case .scheduled(let work, _):
        switch work {
        case .continuation(let continuation): actions.append(.resumeSuccess(continuation))
        case .job(let job): actions.append(.run(job))
        }
      case .cancelling(let continuation, _):
        // Cancelled while pending: resume throwing, now on the executor thread.
        actions.append(.resumeCancelled(continuation))
      case nil:
        continue   // cancelled/stale -> its heap entry is left behind (lazy deletion)
      }
    }
  }

  // Invoked from the C trampoline when `context`'s clock reaches its earliest
  // deadline.  Reuses the context's `actions` scratch buffer (safe because this
  // clock's fire is serialized by Dispatch), so it doesn't allocate.
  func fire(_ context: ClockContext) {
    context.actions.removeAll(keepingCapacity: true)
    let key = BucketKey(clockID: context.clockID, qos: context.qos)
    state.withLock { state in
      guard var queue = state.buckets[key] else { return }
      let now = currentTime(context.clockID)
      collectDue(from: &queue, operations: &state.operations,
                 now: now, into: &context.actions)
      reprogram(&queue)
      state.buckets[key] = queue
    }
    // Perform the collected actions outside the lock.
    for action in context.actions {
      perform(action)
    }
  }
}

@available(StdlibDeploymentTarget 9999, *)
func _dispatchTimerFireTrampoline(_ rawContext: UnsafeMutableRawPointer?) {
  guard let rawContext = unsafe rawContext else { return }
  let context = unsafe Unmanaged<DispatchTimerScheduler.ClockContext>
    .fromOpaque(rawContext).takeUnretainedValue()
  unsafe context.scheduler.fire(context)
}

#endif // !$Embedded && !os(WASI) && !os(Emscripten)
