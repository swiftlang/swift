//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A service that can execute jobs.
@available(SwiftStdlib 5.1, *)
public protocol Executor: AnyObject, Sendable {

  // Since lack move-only type support in the SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY configuration
  // Do not deprecate the UnownedJob enqueue in that configuration just yet - as we cannot introduce the replacements.
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(SwiftStdlib 5.1, *)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  func enqueue(_ job: UnownedJob)

  // Cannot introduce these methods in SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  // since it lacks move-only type support.
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(StdlibDeploymentTarget 5.9, *)
  @available(*, deprecated, message: "Implement 'enqueue(_: consuming ExecutorJob)' instead")
  func enqueue(_ job: consuming Job)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(StdlibDeploymentTarget 5.9, *)
  func enqueue(_ job: consuming ExecutorJob)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
}

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

/// Represents the time at which a scheduled job should run.
///
/// A fire time is either relative or absolute, and provides methods to
/// convert to whichever form a given executor needs.  This lets
/// `SchedulingExecutor` have a single `enqueue` requirement that accepts
/// either a `Duration` or an `Instant`.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
public enum SchedulingExecutorFireTime<C: Clock> {
  /// A time relative to now.
  case after(C.Duration)

  /// An absolute time.
  case at(C.Instant)

  /// Returns the duration from now until the fire time.
  public func asDuration(clock: C) -> C.Duration {
    switch self {
    case let .after(duration):
      return duration
    case let .at(instant):
      return clock.now.duration(to: instant)
    }
  }

  /// Returns the absolute time represented by the fire time.
  public func asInstant(clock: C) -> C.Instant {
    switch self {
    case let .after(duration):
      return clock.now.advanced(by: duration)
    case let .at(instant):
      return instant
    }
  }
}

/// The suspended half of a sleeping task, handed to a `SchedulingExecutor`
/// to resume when its timer fires.
///
/// Scheduling a continuation, rather than a job, lets the executor resume the
/// real task, so that everything the runtime does when it resumes a task
/// (priority handling, tracing, and so on) applies, and no stand-in job needs
/// to be allocated.
///
/// An executor must resume the continuation exactly once, by calling
/// `resume()` when the timer fires; or, if the timer is cancelled first, by
/// returning it from `SchedulingExecutor.cancel(continuationWithToken:)`, in
/// which case the caller resumes it.  It may be resumed from any thread.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
@safe
public struct TimerContinuation: ~Copyable {
  internal var continuation: UnsafeContinuation<Void, _Concurrency.CancellationError>

  internal init(
    _ continuation: UnsafeContinuation<Void, _Concurrency.CancellationError>
  ) {
    unsafe self.continuation = continuation
  }

  /// Wrap a continuation that you hold, for passing to
  /// `SchedulingExecutor.enqueue(resuming:...)`.
  ///
  /// The continuation must be resumed exactly once, which is now the job of
  /// whatever you pass the result to.  The only error it will be resumed with
  /// is `CancellationError`.
  public init(_ continuation: UnsafeContinuation<Void, any Error>) {
    unsafe self.continuation = unsafeBitCast(
      continuation,
      to: UnsafeContinuation<Void, _Concurrency.CancellationError>.self)
  }

  /// Reconstitute a continuation from the bits returned by `_takeRawBits()`.
  internal init(rawBits: Int) {
    unsafe self.continuation = unsafeBitCast(
      rawBits,
      to: UnsafeContinuation<Void, _Concurrency.CancellationError>.self)
  }

  /// Give up ownership, returning the continuation as an integer that can be
  /// passed through a C interface.
  internal consuming func _takeRawBits() -> Int {
    return unsafe unsafeBitCast(continuation, to: Int.self)
  }

  /// Resume the sleeping task normally.
  public consuming func resume() {
    unsafe continuation.resume()
  }

  /// Resume the sleeping task by throwing an error.
  public consuming func resume(throwing error: _Concurrency.CancellationError) {
    unsafe continuation.resume(throwing: error)
  }
}

/// Something that can cancel a job or continuation that it scheduled.
///
/// Every `SchedulingExecutor` is one.  A `Clock` that schedules work itself
/// (see `EnqueueingClock`) can conform to this as well, if it needs to be the
/// target of the cancellation tokens that it hands out.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
public protocol JobCanceller: Sendable {
  /// Cancel a scheduled job.
  ///
  /// If the job identified by the token has not yet started, remove it from
  /// the scheduler and return it, handing ownership to the caller.  The
  /// scheduler must first undo anything it attached to the job so that the
  /// caller may run, enqueue or destroy it.
  ///
  /// Return `nil` only if the job has already started running or has
  /// finished; callers take that to mean the job will run (or has run)
  /// normally.  A scheduler that is unable to remove pending jobs may always
  /// return `nil`, but then cancelling a `Task.sleep()` will not wake the task
  /// until its timer fires, and the timer stays queued until then.
  ///
  /// This may be called from any thread.  A token for a job that has already
  /// run must return `nil` and must never match a different job.
  ///
  /// Parameters:
  ///
  /// - jobWithToken: The token returned when the job was scheduled.
  func cancel(jobWithToken: consuming JobCancellationToken) -> ExecutorJob?

  /// Cancel a scheduled continuation.
  ///
  /// If the timer identified by the token has not fired, remove it and return
  /// its continuation, handing ownership (and the responsibility to resume it)
  /// to the caller.  Return `nil` only if the timer has fired or is firing, in
  /// which case the scheduler resumes the continuation.
  ///
  /// The default implementation cancels the job that the default
  /// `enqueue(resuming:...)` creates, and takes the continuation back out of
  /// it.  Something that implements `enqueue(resuming:...)` or
  /// `run(resuming:...)` natively must implement this too.
  ///
  /// This may be called from any thread.  A token for a timer that has already
  /// fired must return `nil` and must never match a different timer.
  ///
  /// Parameters:
  ///
  /// - continuationWithToken: The token returned when the continuation was
  ///                          scheduled.
  func cancel(continuationWithToken: consuming JobCancellationToken)
    -> TimerContinuation?
}

@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
extension JobCanceller {
  public func cancel(continuationWithToken token: consuming JobCancellationToken)
    -> TimerContinuation? {
    guard let job = cancel(jobWithToken: token) else {
      return nil
    }

    // The job is the stand-in that a default `enqueue(resuming:...)` or
    // `run(resuming:...)` created; take the continuation back out of it.
    let raw = _swiftJobTakeNullaryContinuation(UnownedJob(job))
    return unsafe TimerContinuation(UnsafeContinuation(raw))
  }
}

/// Represents a job that has been scheduled for future execution.
///
/// Executors create these in `SchedulingExecutor.enqueue(...)`.  Calling
/// `cancel()` asks the scheduler to give the job back if it has not yet
/// started running.
///
/// Dropping the token without cancelling does not affect the job; the token's
/// clean-up function (if any) runs so that the executor can release any
/// resources it is keeping for cancellation.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
public struct JobCancellationToken: ~Copyable {
  /// What to ask to cancel the job: the executor that it was scheduled on or,
  /// for work scheduled by a `Clock`, possibly the clock.
  public let canceller: any JobCanceller

  /// An identifier for the job, assigned by the executor.  Executors that
  /// identify jobs by ID (rather than via `opaqueData`) must not reuse them.
  public let jobID: UInt64

  /// Opaque, executor-specific data.
  public let opaqueData: InlineArray<2, Int>

  /// An optional function that is called when the token is destroyed,
  /// whether or not the job was cancelled.
  public let cleanUp: (@convention(thin) (borrowing JobCancellationToken) -> ())?

  public init(canceller: any JobCanceller,
              jobID: UInt64,
              opaqueData: InlineArray<2, Int>,
              cleanUp: (@convention(thin) (borrowing JobCancellationToken) -> ())? = nil) {
    self.canceller = canceller
    self.jobID = jobID
    self.opaqueData = opaqueData
    self.cleanUp = cleanUp
  }

  deinit {
    if let cleanUp {
      cleanUp(self)
    }
  }

  /// Cancel the scheduled job.
  ///
  /// If the job had not started running, it is removed from the executor and
  /// returned; the caller then owns it and must run, enqueue or destroy it.
  /// Returns `nil` if the job has started, has finished, or cannot be
  /// cancelled by the executor, in which case it will run (or has run)
  /// normally.
  public consuming func cancel() -> ExecutorJob? {
    return canceller.cancel(jobWithToken: self)
  }

  /// Cancel a scheduled continuation.
  ///
  /// This is the counterpart of `cancel()` for a token returned by
  /// `SchedulingExecutor.enqueue(resuming:...)`.  If the timer had not fired,
  /// the continuation is returned and the caller must resume it; otherwise
  /// the result is `nil`, and the executor has resumed (or is resuming) it.
  public consuming func cancelContinuation() -> TimerContinuation? {
    return canceller.cancel(continuationWithToken: self)
  }
}

#endif // !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
public protocol SchedulingExecutor: Executor, JobCanceller {

  /// Enqueue a job to run at a specified time.
  ///
  /// Parameters:
  ///
  /// - job:       The job to schedule.
  /// - run:       A `SchedulingExecutorFireTime` saying when the job should
  ///              run.  The job will not run before that time.
  /// - tolerance: The maximum additional delay permissible before the
  ///              job is executed.  `nil` means no limit.
  /// - clock:     The clock used to interpret `run`.
  ///
  /// Returns a token that can be used to cancel the job before it runs.
  @available(StdlibDeploymentTarget 6.5, *)
  func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                         run: SchedulingExecutorFireTime<C>,
                         tolerance: C.Duration?,
                         clock: C) -> JobCancellationToken

  /// Resume a continuation at a specified time.
  ///
  /// This is what `Task.sleep()` uses.  Unlike the `ExecutorJob` form, the
  /// executor doesn't need to create a job to schedule: when the timer fires
  /// it simply resumes the continuation.
  ///
  /// The default implementation creates a job that resumes the continuation
  /// and schedules that using `enqueue(_:run:tolerance:clock:)`.  Executors
  /// that implement this natively must also implement
  /// `cancel(continuationWithToken:)`.
  ///
  /// Parameters:
  ///
  /// - continuation: The continuation to resume.
  /// - run:          A `SchedulingExecutorFireTime` saying when to resume it.
  ///                 It will not be resumed before that time.
  /// - tolerance:    The maximum additional delay permissible.  `nil` means
  ///                 no limit.
  /// - clock:        The clock used to interpret `run`.
  ///
  /// Returns a token that can be used to cancel the timer before it fires.
  @available(StdlibDeploymentTarget 6.5, *)
  func enqueue<C: Clock>(resuming continuation: consuming TimerContinuation,
                         run: SchedulingExecutorFireTime<C>,
                         tolerance: C.Duration?,
                         clock: C) -> JobCancellationToken

}
#else
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
public protocol SchedulingExecutor: Executor {
}
#endif // !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
extension SchedulingExecutor {
  public func enqueue<C: Clock>(resuming continuation: consuming TimerContinuation,
                                run: SchedulingExecutorFireTime<C>,
                                tolerance: C.Duration?,
                                clock: C) -> JobCancellationToken {
    let raw = unsafe continuation.continuation.context
    let job = _taskCreateNullaryContinuationJob(
      priority: Int(Task.currentPriority.rawValue),
      continuation: raw)
    return enqueue(ExecutorJob(context: job),
                   run: run,
                   tolerance: tolerance,
                   clock: clock)
  }
}
#endif // !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

extension Actor {

  /// Perform an operation with the actor's ``SerialExecutor``.
  ///
  /// This converts the actor's ``Actor/unownedExecutor`` to a ``SerialExecutor`` while
  /// retaining the actor for the duration of the operation. This is to ensure the lifetime
  /// of the executor while performing the operation.
  @export(implementation)
  @available(SwiftStdlib 5.1, *)
  public nonisolated func withSerialExecutor<T: ~Copyable, E: Error>(_ operation: (any SerialExecutor) throws(E) -> T) throws(E) -> T {
    try operation(unsafe unsafeBitCast(self.unownedExecutor, to: (any SerialExecutor).self))
  }

  /// Perform an operation with the actor's ``SerialExecutor``.
  ///
  /// This converts the actor's ``Actor/unownedExecutor`` to a ``SerialExecutor`` while
  /// retaining the actor for the duration of the operation. This is to ensure the lifetime
  /// of the executor while performing the operation.
  @export(implementation)
  @available(SwiftStdlib 5.1, *)
  public nonisolated func withSerialExecutor<T: ~Copyable, E: Error>(_ operation: nonisolated(nonsending) (any SerialExecutor) async throws(E) -> T) async throws(E) -> T {
    try await operation(unsafe unsafeBitCast(self.unownedExecutor, to: (any SerialExecutor).self))
  }
}

extension Executor {

  #if !$Embedded
  /// Return this executor as a SchedulingExecutor, or nil if that is
  /// unsupported.
  ///
  /// Executors that implement SchedulingExecutor should provide their
  /// own copy of this method, which will allow the compiler to avoid a
  /// potentially expensive runtime cast.
  #if !os(Windows)
  @_weakLinked
  #endif
  @available(StdlibDeploymentTarget 6.5, *)
  internal var asSchedulingExecutor: (any SchedulingExecutor)? {
    return self as? SchedulingExecutor
  }
  #endif

}

extension Executor {
  @available(SwiftStdlib 6.2, *)
  @usableFromInline
  internal var _isComplexEquality: Bool { false }
}

extension Executor where Self: Equatable {
  @available(SwiftStdlib 6.2, *)
  @usableFromInline
  internal var _isComplexEquality: Bool { true }
}

extension Executor {

  #if os(WASI) || os(Emscripten) || !$Embedded
  // This defaults to `false` so that existing third-party Executor
  // implementations will work as expected.
  #if !os(Windows)
  @_weakLinked
  #endif
  @available(StdlibDeploymentTarget 6.3, *)
  internal var isMainExecutor: Bool {
    #if !$Embedded
    return self is any MainExecutor
    #else
    return false
    #endif
  }
  #endif // os(WASI) || os(Emscripten) || !$Embedded

}

// Delay support
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 6.5, *)
extension SchedulingExecutor {

  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

  /// Enqueue a job to run at a specified time, with no tolerance.
  @available(StdlibDeploymentTarget 6.5, *)
  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                run: SchedulingExecutorFireTime<C>,
                                clock: C) -> JobCancellationToken {
    return enqueue(job, run: run, tolerance: nil, clock: clock)
  }

  #endif // !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
}

/// A service that executes jobs.
///
/// ### Custom Actor Executors
/// By default, all actor types execute tasks on a shared global concurrent pool.
/// The global pool does not guarantee any thread (or dispatch queue) affinity,
/// so actors are free to use different threads as they execute tasks.
///
/// > The runtime may perform various optimizations to minimize un-necessary
/// > thread switching.
///
/// Sometimes it is important to be able to customize the execution behavior
///  of an actor. For example, when an actor is known to perform heavy blocking
/// operations (such as IO), and we would like to keep this work *off* the global
/// shared pool, as blocking it may prevent other actors from being responsive.
///
/// You can implement a custom executor, by conforming a type to the
/// ``SerialExecutor`` protocol, and implementing the ``enqueue(_:)-7sypu`` method.
///
/// Once implemented, you can configure an actor to use such executor by
/// implementing the actor's ``Actor/unownedExecutor`` computed property.
/// For example, you could accept an executor in the actor's initializer,
/// store it as a variable (in order to retain it for the duration of the
/// actor's lifetime), and return it from the `unownedExecutor` computed
/// property like this:
///
/// ```
/// actor MyActor {
///   let myExecutor: MyExecutor
///
///   // accepts an executor to run this actor on.
///   init(executor: MyExecutor) {
///     self.myExecutor = executor
///   }
///
///   nonisolated var unownedExecutor: UnownedSerialExecutor {
///     self.myExecutor.asUnownedSerialExecutor()
///   }
/// }
/// ```
///
/// It is also possible to use a form of shared executor, either created as a
/// global or static property, which you can then re-use for every MyActor
/// instance:
///
/// ```
/// actor MyActor {
///   // Serial executor reused by *all* instances of MyActor!
///   static let sharedMyActorsExecutor = MyExecutor() // implements SerialExecutor
///
///
///   nonisolated var unownedExecutor: UnownedSerialExecutor {
///     Self.sharedMyActorsExecutor.asUnownedSerialExecutor()
///   }
/// }
/// ```
///
/// In the example above, *all* "MyActor" instances would be using the same
/// serial executor, which would result in only one of such actors ever being
/// run at the same time. This may be useful if some of your code has some
/// "specific thread" requirement when interoperating with non-Swift runtimes
/// for example.
///
/// Since the ``UnownedSerialExecutor`` returned by the `unownedExecutor`
/// property *does not* retain the executor, you must make sure the lifetime of
/// it extends beyond the lifetime of any actor or task using it, as otherwise
/// it may attempt to enqueue work on a released executor object, causing a crash.
/// The executor returned by unownedExecutor *must* always be the same object,
/// and returning different executors can lead to unexpected behavior.
///
/// Alternatively, you can also use existing serial executor implementations,
/// such as Dispatch's `DispatchSerialQueue` or others.
@available(SwiftStdlib 5.1, *)
public protocol SerialExecutor: Executor {
  // This requirement is repeated here as a non-override so that we
  // get a redundant witness-table entry for it.  This allows us to
  // avoid drilling down to the base conformance just for the basic
  // work-scheduling operation.
  @_nonoverride
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(SwiftStdlib 5.1, *)
  @available(*, deprecated, message: "Implement 'enqueue(_: consuming ExecutorJob)' instead")
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  func enqueue(_ job: UnownedJob)

  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  // This requirement is repeated here as a non-override so that we
  // get a redundant witness-table entry for it.  This allows us to
  // avoid drilling down to the base conformance just for the basic
  // work-scheduling operation.
  @_nonoverride
  @available(StdlibDeploymentTarget 5.9, *)
  @available(*, deprecated, message: "Implement 'enqueue(_: consuming ExecutorJob)' instead")
  func enqueue(_ job: consuming Job)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  // This requirement is repeated here as a non-override so that we
  // get a redundant witness-table entry for it.  This allows us to
  // avoid drilling down to the base conformance just for the basic
  // work-scheduling operation.
  @_nonoverride
  @available(StdlibDeploymentTarget 5.9, *)
  func enqueue(_ job: consuming ExecutorJob)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

  /// Convert this executor value to the optimized form of borrowed
  /// executor references.
  @unsafe
  func asUnownedSerialExecutor() -> UnownedSerialExecutor

  /// If this executor has complex equality semantics, and the runtime needs to
  /// compare two executors, it will first attempt the usual pointer-based
  /// equality / check, / and if it fails it will compare the types of both
  /// executors, if they are the same, / it will finally invoke this method,
  ///  in an
  /// attempt to let the executor itself decide / if this and the `other`
  /// executor represent the same serial, exclusive, isolation context.
  ///
  /// This method must be implemented with great care, as wrongly returning
  /// `true` would allow / code from a different execution context (e.g. thread)
  /// to execute code which was intended to be isolated by another actor.
  ///
  /// This check is not used when performing executor switching.
  ///
  /// This check is used when performing ``Actor/assertIsolated(_:file:line:)``,
  /// ``Actor/preconditionIsolated(_:file:line:)``,
  /// ``Actor/assumeIsolated(_:file:line:)`` and similar APIs which assert about
  /// the same "exclusive serial execution context".
  ///
  /// - Parameter other: the executor to compare with.
  /// - Returns: `true`, if `self` and the `other` executor actually are
  ///            mutually exclusive and it is safe–from a concurrency
  ///            perspective–to execute code assuming one on the other.
  @available(StdlibDeploymentTarget 5.9, *)
  func isSameExclusiveExecutionContext(other: Self) -> Bool

  /// Last resort "fallback" isolation check, called when the concurrency runtime
  /// is comparing executors e.g. during ``Actor/assumeIsolated(_:file:line:)``
  /// and is unable to prove serial equivalence between the expected
  /// (this object), and the current executor.
  ///
  /// During executor comparison, the Swift concurrency runtime attempts to compare
  /// current and expected executors in a few ways (including "complex" equality
  /// between executors (see ``isSameExclusiveExecutionContext(other:)``), and if all
  /// those checks fail, this method is invoked on the expected executor.
  ///
  /// This method MUST crash if it is unable to prove that the current execution
  /// context belongs to this executor. At this point usual executor comparison would
  /// have already failed, though the executor may have some external tracking of
  /// threads it owns, and may be able to prove isolation nevertheless.
  ///
  /// A default implementation is provided that unconditionally crashes the
  /// program, and prevents calling code from proceeding with potentially
  /// not thread-safe execution.
  ///
  /// - Warning: This method must crash and halt program execution if unable
  ///     to prove the isolation of the calling context.
  @available(StdlibDeploymentTarget 6.0, *)
  func checkIsolated()

  /// Checks if the current execution context is isolated by this executor.
  ///
  /// This function can be called by the runtime in order to perform assertions,
  /// or attempt to issue warnings about unexpected isolation.
  ///
  /// This method will be invoked _before_ `checkIsolated` and may also be invoked
  /// when crashing is not an acceptable outcome of a check (e.g. when attempting to issue isolation _warnings_).
  ///
  /// Implementations should prefer to implement this method rather than `checkIsolated()` since it can often
  /// result in more tailored error messages. It is allowed, and useful for backwards compatibility with old
  /// runtimes which are not able to invoke `isIsolatingCurrentContext()` to implement `checkIsolated()`,
  /// even if an implementation is able to implement this method. Often times an implementation of `checkIsolated()`,
  /// would then invoke `isIsolatingCurrentContext()` and crash if the returned value was `false`.
  ///
  /// The default implementation returns `nil` is used to indicate that it is "unknown" if the current context is
  /// isolated by this serial executor. The runtime then _may_ proceed to invoke `checkIsolated()` as a last-resort
  /// attempt to verify the isolation of the current context.
  @available(StdlibDeploymentTarget 6.2, *)
  func isIsolatingCurrentContext() -> Bool?

}

@available(StdlibDeploymentTarget 6.0, *)
extension SerialExecutor {

  #if os(WASI) || os(Emscripten) || (!$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY)
  @available(StdlibDeploymentTarget 6.3, *)
  public var isMainExecutor: Bool { return MainActor.executor._isSameExecutor(self) }
  #endif // os(WASI) || os(Emscripten) || (!$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY)

  @available(StdlibDeploymentTarget 6.0, *)
  public func checkIsolated() {
    #if !$Embedded
    fatalError("Unexpected isolation context, expected to be executing on \(Self.self)")
    #else
    Builtin.int_trap()
    #endif
  }

  #if SWIFT_CONCURRENCY_USES_DISPATCH
  @available(StdlibDeploymentTarget 6.3, *)
  private var _dispatchQueue: OpaquePointer? {
    return unsafe _getDispatchQueueForExecutor(self.asUnownedSerialExecutor())
  }
  #endif

  @available(StdlibDeploymentTarget 6.3, *)
  internal func _isSameExecutor(_ rhs: some SerialExecutor) -> Bool {
    if rhs === self {
      return true
    }
    #if SWIFT_CONCURRENCY_USES_DISPATCH
    if let rhsQueue = unsafe rhs._dispatchQueue {
      if let ourQueue = unsafe _dispatchQueue, ourQueue == rhsQueue {
        return true
      }
      return false
    }
    #endif
    if let rhs = rhs as? Self {
      return isSameExclusiveExecutionContext(other: rhs)
    }
    return false
  }
}

@available(StdlibDeploymentTarget 6.2, *)
extension SerialExecutor {

  @available(StdlibDeploymentTarget 6.2, *)
  public func isIsolatingCurrentContext() -> Bool? {
    return nil
  }
}

/// An executor that may be used as preferred executor by a task.
///
/// ### Impact of setting a task executor preference
/// By default, without setting a task executor preference, nonisolated
/// asynchronous functions, as well as methods declared on default actors --
/// that is actors which do not require a specific executor -- execute on
/// Swift's default global concurrent executor. This is an executor shared by
/// the entire runtime to execute any work which does not have strict executor
/// requirements.
///
/// By setting a task executor preference, either with a
/// ``withTaskExecutorPreference(_:isolation:operation:)`` or creating a task
/// with a preference -- using `Task(executorPreference:)` or
/// `group.addTask(executorPreference:)` -- the task and all of its child tasks
/// (unless a new preference is set) will be preferring to execute on
/// the provided task executor.
///
/// Unstructured tasks do not inherit the task executor.
@available(StdlibDeploymentTarget 6.0, *)
public protocol TaskExecutor: Executor {
  // This requirement is repeated here as a non-override so that we
  // get a redundant witness-table entry for it.  This allows us to
  // avoid drilling down to the base conformance just for the basic
  // work-scheduling operation.
  @_nonoverride
  func enqueue(_ job: UnownedJob)

  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  // This requirement is repeated here as a non-override so that we
  // get a redundant witness-table entry for it.  This allows us to
  // avoid drilling down to the base conformance just for the basic
  // work-scheduling operation.
  @_nonoverride
  @available(*, deprecated, message: "Implement 'enqueue(_: consuming ExecutorJob)' instead")
  func enqueue(_ job: consuming Job)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  // This requirement is repeated here as a non-override so that we
  // get a redundant witness-table entry for it.  This allows us to
  // avoid drilling down to the base conformance just for the basic
  // work-scheduling operation.
  @_nonoverride
  func enqueue(_ job: consuming ExecutorJob)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

  func asUnownedTaskExecutor() -> UnownedTaskExecutor
}

@available(StdlibDeploymentTarget 6.0, *)
extension TaskExecutor {
  public func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    unsafe UnownedTaskExecutor(ordinary: self)
  }
}

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(StdlibDeploymentTarget 5.9, *)
extension Executor {

  // Delegation goes like this:
  // Unowned Job -> Executor Job -> Job -> ---||---

  @inlinable
  public func enqueue(_ job: UnownedJob) {
    self.enqueue(ExecutorJob(job))
  }

  @inlinable
  @diagnose(DeprecatedDeclaration, as: ignored)
  public func enqueue(_ job: consuming ExecutorJob) {
    self.enqueue(Job(job))
  }

  @inlinable
  @diagnose(DeprecatedDeclaration, as: ignored)
  public func enqueue(_ job: consuming Job) {
    self.enqueue(UnownedJob(job))
  }
}
#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

@available(StdlibDeploymentTarget 5.9, *)
extension SerialExecutor {
  @available(StdlibDeploymentTarget 5.9, *)
  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    unsafe UnownedSerialExecutor(ordinary: self)
  }
}

@available(StdlibDeploymentTarget 5.9, *)
extension SerialExecutor {

  @available(StdlibDeploymentTarget 5.9, *)
  public func isSameExclusiveExecutionContext(other: Self) -> Bool {
    return self === other
  }

}

@available(StdlibDeploymentTarget 6.3, *)
extension SerialExecutor where Self: Equatable {

  @available(StdlibDeploymentTarget 6.3, *)
  public func isSameExclusiveExecutionContext(other: Self) -> Bool {
    return self == other
  }

}

/// An executor that is backed by some kind of run loop.
///
/// The idea here is that some executors may work by running a loop
/// that processes events of some sort; we want a way to enter that loop,
/// and we would also like a way to trigger the loop to exit.
@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
public protocol RunLoopExecutor: Executor {
  /// Run the executor's run loop.
  ///
  /// This method will synchronously block the calling thread.  Nested calls to
  /// `run()` may be permitted, however it is not permitted to call `run()` on a
  /// single executor instance from more than one thread.
  func run() throws

  /// Run the executor's run loop until a condition is satisfied.
  ///
  /// Not every `RunLoopExecutor` will support this method; you must not call
  /// it unless you *know* that it is supported.  The default implementation
  /// generates a fatal error.
  ///
  /// Parameters:
  ///
  /// - condition: A closure that returns `true` if the run loop should
  ///              stop.
  func runUntil(_ condition: () -> Bool) throws

  /// Signal to the run loop to stop running and return.
  ///
  /// This method may be called from the same thread that is in the `run()`
  /// method, or from some other thread.  It will not wait for the run loop to
  /// stop; calling this method simply signals that the run loop *should*, as
  /// soon as is practicable, stop the innermost `run()` invocation and make
  /// that `run()` invocation return.
  func stop()
}

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
extension RunLoopExecutor {

  public func runUntil(_ condition: () -> Bool) throws {
    fatalError("run(until condition:) not supported on this executor")
  }

}


/// The main executor must conform to these two protocols; we have to
/// make this a protocol for compatibility with Embedded Swift.
@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
public protocol MainExecutor: RunLoopExecutor, SerialExecutor {
}


/// An ExecutorFactory is used to create the default main and task
/// executors.
@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
public protocol ExecutorFactory {
  #if os(WASI) || os(Emscripten) || !$Embedded
  /// Constructs and returns the main executor, which is started implicitly
  /// by the `async main` entry point and owns the "main" thread.
  static var mainExecutor: any MainExecutor { get }
  #endif // os(WASI) || os(Emscripten) || !$Embedded

  /// Constructs and returns the default or global executor, which is the
  /// default place in which we run tasks.
  static var defaultExecutor: any TaskExecutor { get }
}

@available(StdlibDeploymentTarget 6.3, *)
typealias DefaultExecutorFactory = PlatformExecutorFactory

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_createExecutors")
public func _createExecutors<F: ExecutorFactory>(factory: F.Type) {
  #if os(WASI) || os(Emscripten) || (!$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY)
  MainActor._executor = factory.mainExecutor
  #endif // os(WASI) || os(Emscripten) || (!$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY)
  Task._defaultExecutor = factory.defaultExecutor
}

@available(SwiftStdlib 6.3, *)
@_silgen_name("swift_createDefaultExecutors")
func _createDefaultExecutors() {
  if Task._defaultExecutor == nil {
    _createExecutors(factory: DefaultExecutorFactory.self)
  }
}

#if os(WASI) || os(Emscripten) || (!$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY)
extension MainActor {
  @available(StdlibDeploymentTarget 6.3, *)
  static var _executor: (any MainExecutor)? = nil

  /// The main executor, which is started implicitly by the `async main`
  /// entry point and owns the "main" thread.
  ///
  /// Attempting to set this after the first `enqueue` on the main
  /// executor is a fatal error.
  @_spi(ExperimentalCustomExecutors)
  @available(StdlibDeploymentTarget 6.3, *)
  public static var executor: any MainExecutor {
    // It would be good if there was a Swift way to do this
    _createDefaultExecutorsOnce()
    return _executor!
  }

  /// An unowned version of the above, for performance
  @available(StdlibDeploymentTarget 6.3, *)
  static var unownedExecutor: UnownedSerialExecutor {
    _createDefaultExecutorsOnce()
    return unsafe UnownedSerialExecutor(ordinary: _executor!)
  }
}
#endif // os(WASI) || os(Emscripten) || (!$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY)

extension Task where Success == Never, Failure == Never {
  @available(StdlibDeploymentTarget 6.3, *)
  static var _defaultExecutor: (any TaskExecutor)? = nil

  /// The default or global executor, which is the default place in which
  /// we run tasks.
  ///
  /// Attempting to set this after the first `enqueue` on the global
  /// executor is a fatal error.
  @_spi(ExperimentalCustomExecutors)
  @available(StdlibDeploymentTarget 6.3, *)
  public static var defaultExecutor: any TaskExecutor {
    // It would be good if there was a Swift way to do this
    _createDefaultExecutorsOnce()
    return _defaultExecutor!
  }

  /// An unowned version of the above, for performance
  @available(StdlibDeploymentTarget 6.3, *)
  static var unownedDefaultExecutor: UnownedTaskExecutor {
    _createDefaultExecutorsOnce()
    return unsafe UnownedTaskExecutor(Builtin.buildOrdinaryTaskExecutorRef(_defaultExecutor!))
  }
}

extension Task where Success == Never, Failure == Never {
  /// Get the current executor; this is the executor that the currently
  /// executing task is executing on.
  ///
  /// This will return, in order of preference:
  ///
  ///   1. The custom executor associated with an `Actor` on which we are
  ///      currently running, or
  ///   2. The preferred executor for the currently executing `Task`, or
  ///   3. The task executor for the current thread
  ///
  ///  If none of these exist, returns the default executor.
  @available(StdlibDeploymentTarget 6.3, *)
  @_unavailableInEmbedded
  static var currentExecutor: any Executor {
    if let activeExecutor = unsafe _getActiveExecutor().asSerialExecutor() {
      return activeExecutor
    } else if let taskExecutor = unsafe _getPreferredTaskExecutor().asTaskExecutor() {
      return taskExecutor
    } else if let taskExecutor = unsafe _getCurrentTaskExecutor().asTaskExecutor() {
      return taskExecutor
    }
    return defaultExecutor
  }

  /// Get the preferred executor for the current `Task`, if any.
  @available(StdlibDeploymentTarget 6.3, *)
  static var preferredExecutor: (any TaskExecutor)? {
    if let taskExecutor = unsafe _getPreferredTaskExecutor().asTaskExecutor() {
      return taskExecutor
    }
    return nil
  }

  #if !$Embedded
  /// Get the current *scheduling* executor, if any.
  ///
  /// This follows the same logic as `currentExecutor`, except that it ignores
  /// any executor that isn't a `SchedulingExecutor`.
  @available(StdlibDeploymentTarget 6.5, *)
  static var currentSchedulingExecutor: (any SchedulingExecutor)? {
    if let activeExecutor = unsafe _getActiveExecutor().asSerialExecutor(),
       let scheduling = activeExecutor.asSchedulingExecutor {
      return scheduling
    }
    if let taskExecutor = unsafe _getPreferredTaskExecutor().asTaskExecutor(),
       let scheduling = taskExecutor.asSchedulingExecutor {
      return scheduling
    }
    if let taskExecutor = unsafe _getCurrentTaskExecutor().asTaskExecutor(),
       let scheduling = taskExecutor.asSchedulingExecutor {
      return scheduling
    }
    if let scheduling = defaultExecutor.asSchedulingExecutor {
      return scheduling
    }
    return nil
  }
  #endif

}


/// An unowned reference to a serial executor (a `SerialExecutor`
/// value).
///
/// This is an optimized type used internally by the core scheduling
/// operations.  It is an unowned reference to avoid unnecessary
/// reference-counting work even when working with actors abstractly.
/// Generally there are extra constraints imposed on core operations
/// in order to allow this.  For example, keeping an actor alive must
/// also keep the actor's associated executor alive; if they are
/// different objects, the executor must be referenced strongly by the
/// actor.
@available(SwiftStdlib 5.1, *)
@unsafe
@frozen
public struct UnownedSerialExecutor: Sendable {
  @usableFromInline
  internal var executor: Builtin.Executor

  /// SPI: Do not use. Cannot be marked @_spi, since we need to use it from Distributed module
  /// which needs to reach for this from an @_transparent function which prevents @_spi use.
  @available(SwiftStdlib 5.9, *)
  public var _executor: Builtin.Executor {
    unsafe self.executor
  }

  @inlinable
  public init(_ executor: Builtin.Executor) {
    unsafe self.executor = executor
  }

  @inlinable
  public init<E: SerialExecutor>(ordinary executor: __shared E) {
    unsafe self.executor = Builtin.buildOrdinarySerialExecutorRef(executor)
  }

  /// Opts the executor into complex "same exclusive execution context" equality checks.
  ///
  /// This means what when asserting or assuming executors, and the current and expected
  /// executor are not the same instance (by object equality), the runtime may invoke
  /// `isSameExclusiveExecutionContext` in order to compare the executors for equality.
  ///
  /// Implementing such complex equality can be useful if multiple executor instances
  /// actually use the same underlying serialization context and can be therefore
  /// safely treated as the same serial exclusive execution context (e.g. multiple
  /// dispatch queues targeting the same serial queue).
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public init<E: SerialExecutor>(complexEquality executor: __shared E) {
    unsafe self.executor = Builtin.buildComplexEqualitySerialExecutorRef(executor)
  }

  /// Automatically opt-in to complex equality semantics if the Executor
  /// implements `Equatable`.
  @available(SwiftStdlib 6.3, *)
  @inlinable
  public init<E: SerialExecutor>(_ executor: __shared E) {
    if executor._isComplexEquality {
      unsafe self.executor = Builtin.buildComplexEqualitySerialExecutorRef(executor)
    } else {
      unsafe self.executor = Builtin.buildOrdinarySerialExecutorRef(executor)
    }
  }

  @_spi(ConcurrencyExecutors)
  @available(SwiftStdlib 5.9, *)
  public var _isComplexEquality: Bool {
    unsafe _executor_isComplexEquality(self)
  }

  @available(StdlibDeploymentTarget 6.3, *)
  public func asSerialExecutor() -> (any SerialExecutor)? {
    // The low bits of the witness table are used to encode the executor kind.
    // any SerialExecutor needs a raw witness table pointer, so mask off the low
    // bits, knowing the witness table pointer is aligned to the pointer size.
    let rawExecutorData = unsafe unsafeBitCast(executor, to: (UInt, UInt).self)
    let mask = unsafe ~(UInt(MemoryLayout<UnsafeRawPointer>.alignment) - 1)
    let alignedExecutor = (rawExecutorData.0, rawExecutorData.1 & mask)
    return unsafe unsafeBitCast(alignedExecutor, to: (any SerialExecutor)?.self)
  }
}


@available(StdlibDeploymentTarget 6.0, *)
@unsafe
@frozen
public struct UnownedTaskExecutor: Sendable {
  @usableFromInline
  internal var executor: Builtin.Executor

  /// SPI: Do not use. Cannot be marked @_spi, since we need to use it from Distributed module
  /// which needs to reach for this from an @_transparent function which prevents @_spi use.
  @available(SwiftStdlib 6.0, *)
  public var _executor: Builtin.Executor {
    unsafe self.executor
  }

  @inlinable
  public init(_ executor: Builtin.Executor) {
    unsafe self.executor = executor
  }

  @inlinable
  public init<E: TaskExecutor>(ordinary executor: __shared E) {
    unsafe self.executor = Builtin.buildOrdinaryTaskExecutorRef(executor)
  }

  @available(StdlibDeploymentTarget 6.3, *)
  @inlinable
  public init<E: TaskExecutor>(_ executor: __shared E) {
    unsafe self.executor = Builtin.buildOrdinaryTaskExecutorRef(executor)
  }

  @available(StdlibDeploymentTarget 6.3, *)
  public func asTaskExecutor() -> (any TaskExecutor)? {
    return unsafe unsafeBitCast(executor, to: (any TaskExecutor)?.self)
  }
}

@available(SwiftStdlib 6.0, *)
extension UnownedTaskExecutor: Equatable {
  @inlinable
  public static func == (_ lhs: UnownedTaskExecutor, _ rhs: UnownedTaskExecutor) -> Bool {
    unsafe unsafeBitCast(lhs.executor, to: (Int, Int).self) == unsafeBitCast(rhs.executor, to: (Int, Int).self)
  }
}

@_unavailableInEmbedded
@available(SwiftStdlib 6.0, *)
extension UnownedTaskExecutor {
  /// Hash the executor identity into the given hasher.
  ///
  /// This function is available independently from the `Hashable` conformance,
  /// allowing back-deployment to older runtimes when implementing `Hashable`
  /// in user code
  @export(implementation)
  public func hash(into hasher: inout Hasher) {
    let (ident, impl) = unsafe unsafeBitCast(self.executor, to: (Int, Int).self)
    hasher.combine(ident)
    hasher.combine(impl)
  }
}

@_unavailableInEmbedded
@available(SwiftStdlib 6.4, *)
extension UnownedTaskExecutor: Hashable {}

/// Returns either `true` or will CRASH if called from a different executor
/// than the passed `executor`.
///
/// This method will attempt to verify the current executor against `executor`,
/// and as a last-resort call through to `SerialExecutor.checkIsolated`.
///
/// This method will never return `false`. It either can verify we're on the
/// correct executor, or will crash the program. It should be used in
/// isolation correctness guaranteeing APIs.
///
/// Generally, Swift programs should be constructed such that it is statically
/// known that a specific executor is used, for example by using global actors or
/// custom executors. However, in some APIs it may be useful to provide an
/// additional runtime check for this, especially when moving towards Swift
/// concurrency from other runtimes which frequently use such assertions.
///
/// - Parameter executor: The expected executor.
@_spi(ConcurrencyExecutors)
@available(SwiftStdlib 5.9, *)
@_silgen_name("swift_task_isOnExecutor") // This function will CRASH rather than return `false`!
public func _taskIsOnExecutor<Executor: SerialExecutor>(_ executor: Executor) -> Bool

@_spi(ConcurrencyExecutors)
@available(SwiftStdlib 5.9, *)
@_silgen_name("swift_executor_isComplexEquality")
public func _executor_isComplexEquality(_ executor: UnownedSerialExecutor) -> Bool

@available(SwiftStdlib 5.1, *)
@_transparent
public // COMPILER_INTRINSIC
func _checkExpectedExecutor(_filenameStart: Builtin.RawPointer,
                            _filenameLength: Builtin.Word,
                            _filenameIsASCII: Builtin.Int1,
                            _line: Builtin.Word,
                            _executor: Builtin.Executor) {
  if _taskIsCurrentExecutor(_executor) {
    return
  }

  _reportUnexpectedExecutor(
      _filenameStart, _filenameLength, _filenameIsASCII, _line, _executor)
}

/// Primarily a debug utility.
///
/// If the passed in ExecutorJob is a Task, returns the complete 64bit TaskId,
/// otherwise returns only the job's 32bit Id.
///
/// - Returns: the Id stored in this ExecutorJob or Task, for purposes of debug printing
@available(StdlibDeploymentTarget 5.9, *)
@usableFromInline
@_silgen_name("swift_task_getJobTaskId")
internal func _getJobTaskId(_ job: UnownedJob) -> UInt64

/// Returns the 64-bit TaskID of the currently-executing task, or 0 if there
/// is no current task.
@available(StdlibDeploymentTarget 6.5, *)
@usableFromInline
@_silgen_name("swift_task_getCurrentTaskId")
internal func _getCurrentTaskId() -> UInt64

@available(SwiftStdlib 5.9, *)
@_silgen_name("_task_serialExecutor_isSameExclusiveExecutionContext")
internal func _task_serialExecutor_isSameExclusiveExecutionContext<E>(current currentExecutor: E, executor: E) -> Bool
    where E: SerialExecutor {
  currentExecutor.isSameExclusiveExecutionContext(other: executor)
}

#if !$Embedded
@available(SwiftStdlib 6.0, *)
#endif
@_silgen_name("_task_serialExecutor_checkIsolated")
internal func _task_serialExecutor_checkIsolated<E>(executor: E)
    where E: SerialExecutor {
  executor.checkIsolated()
}

#if !$Embedded
@available(SwiftStdlib 6.2, *)
#endif
@_silgen_name("_task_serialExecutor_isIsolatingCurrentContext")
internal func _task_serialExecutor_isIsolatingCurrentContext<E>(executor: E) -> Int8
  where E: SerialExecutor {
  switch executor.isIsolatingCurrentContext() {
  case nil: -1 // unknown
  case .some(false): 0 // not isolated
  case .some(true): 1 // isolated!
  }
}

/// Obtain the executor ref by calling the executor's `asUnownedSerialExecutor()`.
/// The obtained executor ref will have all the user-defined flags set on the executor.
#if !$Embedded
@available(SwiftStdlib 5.9, *)
#endif
@_silgen_name("_task_serialExecutor_getExecutorRef")
internal func _task_serialExecutor_getExecutorRef<E>(_ executor: E) -> Builtin.Executor
    where E: SerialExecutor {
  return unsafe executor.asUnownedSerialExecutor().executor
}

/// Obtain the executor ref by calling the executor's `asUnownedTaskExecutor()`.
/// The obtained executor ref will have all the user-defined flags set on the executor.
#if !$Embedded
@available(SwiftStdlib 6.0, *)
#endif
@_silgen_name("_task_taskExecutor_getTaskExecutorRef")
internal func _task_taskExecutor_getTaskExecutorRef<E>(_ taskExecutor: E) -> Builtin.Executor
    where E: TaskExecutor {
  return unsafe taskExecutor.asUnownedTaskExecutor().executor
}

#if $Embedded
@_silgen_name("_swift_task_enqueueOnExecutor")
internal func _enqueueOnExecutor(job unownedJob: UnownedJob, executor: any SerialExecutor) {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  if #available(StdlibDeploymentTarget 5.9, *) {
    executor.enqueue(ExecutorJob(context: unownedJob._context))
  } else {
    fatalError("we shouldn't get here; if we have, availability is broken")
  }
  #else // SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  executor.enqueue(unownedJob)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
}
#else
// Used by the concurrency runtime
@available(SwiftStdlib 5.1, *)
@_silgen_name("_swift_task_enqueueOnExecutor")
internal func _enqueueOnExecutor<E>(job unownedJob: UnownedJob, executor: E)
where E: SerialExecutor {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  if #available(StdlibDeploymentTarget 5.9, *) {
    executor.enqueue(ExecutorJob(context: unownedJob._context))
  } else {
    fatalError("we shouldn't get here; if we have, availability is broken")
  }
  #else // SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  executor.enqueue(unownedJob)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
}
#endif // #if $Embedded

@_unavailableInEmbedded
@available(SwiftStdlib 6.0, *)
@_silgen_name("_swift_task_enqueueOnTaskExecutor")
internal func _enqueueOnTaskExecutor<E>(job unownedJob: UnownedJob, executor: E) where E: TaskExecutor {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  executor.enqueue(ExecutorJob(context: unownedJob._context))
  #else // SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  executor.enqueue(unownedJob)
  #endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
}

#if SWIFT_CONCURRENCY_USES_DISPATCH
// This must take a DispatchQueueShim, not something like AnyObject,
// or else SILGen will emit a retain/release in unoptimized builds,
// which won't work because DispatchQueues aren't actually
// Swift-retainable.
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_enqueueOnDispatchQueue")
internal func _enqueueOnDispatchQueue(_ job: UnownedJob,
                                      queue: DispatchQueueShim)

/// Used by the runtime solely for the witness table it produces.
/// FIXME: figure out some way to achieve that which doesn't generate
/// all the other metadata
///
/// Expected to work for any primitive dispatch queue; note that this
/// means a dispatch_queue_t, which is not the same as DispatchQueue
/// on platforms where that is an instance of a wrapper class.
@available(SwiftStdlib 5.1, *)
internal final class DispatchQueueShim: @unchecked Sendable, SerialExecutor {
  func enqueue(_ job: UnownedJob) {
    _enqueueOnDispatchQueue(job, queue: self)
  }

  func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    return unsafe UnownedSerialExecutor(ordinary: self)
  }
}
#endif // SWIFT_CONCURRENCY_USES_DISPATCH

@available(SwiftStdlib 6.1, *)
@_silgen_name("swift_task_deinitOnExecutor")
@usableFromInline
internal func _deinitOnExecutor(_ object: __owned AnyObject,
                                _ work: @convention(thin) (__owned AnyObject) -> Void,
                                _ executor: Builtin.Executor,
                                _ flags: Builtin.Word)
