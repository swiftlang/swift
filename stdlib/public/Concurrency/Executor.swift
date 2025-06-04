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

  #if !$Embedded
  /// `true` if this is the main executor.
  @available(StdlibDeploymentTarget 6.2, *)
  var isMainExecutor: Bool { get }
  #endif
}

@available(StdlibDeploymentTarget 6.2, *)
public protocol SchedulableExecutor: Executor {

  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

  /// Enqueue a job to run after a specified delay.
  ///
  /// You need only implement one of the two enqueue functions here;
  /// the default implementation for the other will then call the one
  /// you have implemented.
  ///
  /// Parameters:
  ///
  /// - job:       The job to schedule.
  /// - after:     A `Duration` specifying the time after which the job
  ///              is to run.  The job will not be executed before this
  ///              time has elapsed.
  /// - tolerance: The maximum additional delay permissible before the
  ///              job is executed.  `nil` means no limit.
  /// - clock:     The clock used for the delay.
  @available(StdlibDeploymentTarget 6.2, *)
  func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                         after delay: C.Duration,
                         tolerance: C.Duration?,
                         clock: C)

  /// Enqueue a job to run at a specified time.
  ///
  /// You need only implement one of the two enqueue functions here;
  /// the default implementation for the other will then call the one
  /// you have implemented.
  ///
  /// Parameters:
  ///
  /// - job:       The job to schedule.
  /// - at:        The `Instant` at which the job should run.  The job
  ///              will not be executed before this time.
  /// - tolerance: The maximum additional delay permissible before the
  ///              job is executed.  `nil` means no limit.
  /// - clock:     The clock used for the delay..
  @available(StdlibDeploymentTarget 6.2, *)
  func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                         at instant: C.Instant,
                         tolerance: C.Duration?,
                         clock: C)

  #endif // !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

}

extension Actor {

  // FIXME: SE evolution: these should be removed in favor of an 'executor' property
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public nonisolated func _withSerialExecutor<T>(_ operation: (any SerialExecutor) throws -> T) rethrows -> T {
    try operation(unsafe unsafeBitCast(self.unownedExecutor, to: (any SerialExecutor).self))
  }

  // FIXME: SE evolution: these should be removed in favor of an 'executor' property
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 5.1, *)
  public nonisolated func _withSerialExecutor<T>(_ operation: (any SerialExecutor) async throws -> T) async rethrows -> T {
    try await operation(unsafe unsafeBitCast(self.unownedExecutor, to: (any SerialExecutor).self))
  }
}

extension Executor {
  /// Return this executable as a SchedulableExecutor, or nil if that is
  /// unsupported.
  ///
  /// Executors that implement SchedulableExecutor should provide their
  /// own copy of this method, which will allow the compiler to avoid a
  /// potentially expensive runtime cast.
  @available(StdlibDeploymentTarget 6.2, *)
  var asSchedulable: SchedulableExecutor? {
    return self as? SchedulableExecutor
  }
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

  #if !$Embedded
  // This defaults to `false` so that existing third-party Executor
  // implementations will work as expected.
  @available(StdlibDeploymentTarget 6.2, *)
  public var isMainExecutor: Bool { false }
  #endif

}

// Delay support
@available(StdlibDeploymentTarget 6.2, *)
extension SchedulableExecutor {

  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

  @available(StdlibDeploymentTarget 6.2, *)
  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    // If you crash here with a mutual recursion, it's because you didn't
    // implement one of these two functions
    enqueue(job, at: clock.now.advanced(by: delay),
            tolerance: tolerance, clock: clock)
  }

  @available(StdlibDeploymentTarget 6.2, *)
  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                at instant: C.Instant,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    // If you crash here with a mutual recursion, it's because you didn't
    // implement one of these two functions
    enqueue(job, after: clock.now.duration(to: instant),
            tolerance: tolerance, clock: clock)
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
/// ``SerialExecutor`` protocol, and implementing the ``enqueue(_:)`` method.
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
  /// This check is used when performing ``Actor/assertIsolated()``,
  /// ``Actor/preconditionIsolated()``, ``Actor/assumeIsolated()`` and similar
  /// APIs which assert about the same "exclusive serial execution context".
  ///
  /// - Parameter other: the executor to compare with.
  /// - Returns: `true`, if `self` and the `other` executor actually are
  ///            mutually exclusive and it is safe–from a concurrency
  ///            perspective–to execute code assuming one on the other.
  @available(StdlibDeploymentTarget 5.9, *)
  func isSameExclusiveExecutionContext(other: Self) -> Bool

  /// Last resort "fallback" isolation check, called when the concurrency runtime
  /// is comparing executors e.g. during ``assumeIsolated()`` and is unable to prove
  /// serial equivalence between the expected (this object), and the current executor.
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

  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @available(StdlibDeploymentTarget 6.2, *)
  public var isMainExecutor: Bool { return MainActor.executor._isSameExecutor(self) }
  #endif

  @available(StdlibDeploymentTarget 6.0, *)
  public func checkIsolated() {
    #if !$Embedded
    fatalError("Unexpected isolation context, expected to be executing on \(Self.self)")
    #else
    Builtin.int_trap()
    #endif
  }

  #if SWIFT_CONCURRENCY_USES_DISPATCH
  @available(StdlibDeploymentTarget 6.2, *)
  private var _dispatchQueue: OpaquePointer? {
    return unsafe _getDispatchQueueForExecutor(self.asUnownedSerialExecutor())
  }
  #endif

  @available(StdlibDeploymentTarget 6.2, *)
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
/// ``withTaskExecutorPreference(_:operation:)``, creating a task with a preference
/// (`Task(executorPreference:)`, or `group.addTask(executorPreference:)`), the task and all of its child
/// tasks (unless a new preference is set) will be preferring to execute on
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

  public func enqueue(_ job: UnownedJob) {
    self.enqueue(ExecutorJob(job))
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    self.enqueue(Job(job))
  }

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

@available(StdlibDeploymentTarget 6.2, *)
extension SerialExecutor where Self: Equatable {

  @available(StdlibDeploymentTarget 6.2, *)
  public func isSameExclusiveExecutionContext(other: Self) -> Bool {
    return self == other
  }

}

/// An executor that is backed by some kind of run loop.
///
/// The idea here is that some executors may work by running a loop
/// that processes events of some sort; we want a way to enter that loop,
/// and we would also like a way to trigger the loop to exit.
@available(StdlibDeploymentTarget 6.2, *)
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

@available(StdlibDeploymentTarget 6.2, *)
extension RunLoopExecutor {

  public func runUntil(_ condition: () -> Bool) throws {
    fatalError("run(until condition:) not supported on this executor")
  }

}


/// The main executor must conform to these three protocols; we have to
/// make this a protocol for compatibility with Embedded Swift.
@available(StdlibDeploymentTarget 6.2, *)
public protocol MainExecutor: RunLoopExecutor, SerialExecutor {
}


/// An ExecutorFactory is used to create the default main and task
/// executors.
@available(StdlibDeploymentTarget 6.2, *)
public protocol ExecutorFactory {
  #if !$Embedded
  /// Constructs and returns the main executor, which is started implicitly
  /// by the `async main` entry point and owns the "main" thread.
  static var mainExecutor: any MainExecutor { get }
  #endif

  /// Constructs and returns the default or global executor, which is the
  /// default place in which we run tasks.
  static var defaultExecutor: any TaskExecutor { get }
}

@available(StdlibDeploymentTarget 6.2, *)
typealias DefaultExecutorFactory = PlatformExecutorFactory

@available(StdlibDeploymentTarget 6.2, *)
@_silgen_name("swift_createExecutors")
public func _createExecutors<F: ExecutorFactory>(factory: F.Type) {
  #if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  MainActor._executor = factory.mainExecutor
  #endif
  Task._defaultExecutor = factory.defaultExecutor
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_createDefaultExecutors")
func _createDefaultExecutors() {
  if Task._defaultExecutor == nil {
    _createExecutors(factory: DefaultExecutorFactory.self)
  }
}

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
extension MainActor {
  @available(StdlibDeploymentTarget 6.2, *)
  static var _executor: (any MainExecutor)? = nil

  /// The main executor, which is started implicitly by the `async main`
  /// entry point and owns the "main" thread.
  ///
  /// Attempting to set this after the first `enqueue` on the main
  /// executor is a fatal error.
  @available(StdlibDeploymentTarget 6.2, *)
  public static var executor: any MainExecutor {
    // It would be good if there was a Swift way to do this
    _createDefaultExecutorsOnce()
    return _executor!
  }
}
#endif // !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

extension Task where Success == Never, Failure == Never {
  @available(StdlibDeploymentTarget 6.2, *)
  static var _defaultExecutor: (any TaskExecutor)? = nil

  /// The default or global executor, which is the default place in which
  /// we run tasks.
  ///
  /// Attempting to set this after the first `enqueue` on the global
  /// executor is a fatal error.
  @available(StdlibDeploymentTarget 6.2, *)
  public static var defaultExecutor: any TaskExecutor {
    // It would be good if there was a Swift way to do this
    _createDefaultExecutorsOnce()
    return _defaultExecutor!
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
  @available(StdlibDeploymentTarget 6.2, *)
  @_unavailableInEmbedded
  public static var currentExecutor: any Executor {
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
  @available(StdlibDeploymentTarget 6.2, *)
  public static var preferredExecutor: (any TaskExecutor)? {
    if let taskExecutor = unsafe _getPreferredTaskExecutor().asTaskExecutor() {
      return taskExecutor
    }
    return nil
  }

  /// Get the current *schedulable* executor, if any.
  ///
  /// This follows the same logic as `currentExecutor`, except that it ignores
  /// any executor that isn't a `SchedulableExecutor`.
  @available(StdlibDeploymentTarget 6.2, *)
  @_unavailableInEmbedded
  public static var currentSchedulableExecutor: (any SchedulableExecutor)? {
    if let activeExecutor = unsafe _getActiveExecutor().asSerialExecutor(),
       let schedulable = activeExecutor.asSchedulable {
      return schedulable
    }
    if let taskExecutor = unsafe _getPreferredTaskExecutor().asTaskExecutor(),
       let schedulable = taskExecutor.asSchedulable {
      return schedulable
    }
    if let taskExecutor = unsafe _getCurrentTaskExecutor().asTaskExecutor(),
       let schedulable = taskExecutor.asSchedulable {
      return schedulable
    }
    if let schedulable = defaultExecutor.asSchedulable {
      return schedulable
    }
    return nil
  }
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
  @available(SwiftStdlib 6.2, *)
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

  @available(StdlibDeploymentTarget 6.2, *)
  public func asSerialExecutor() -> (any SerialExecutor)? {
    return unsafe unsafeBitCast(executor, to: (any SerialExecutor)?.self)
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

  @available(SwiftStdlib 6.2, *)
  @inlinable
  public init<E: TaskExecutor>(_ executor: __shared E) {
    unsafe self.executor = Builtin.buildOrdinaryTaskExecutorRef(executor)
  }

  @available(StdlibDeploymentTarget 6.2, *)
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
@_silgen_name("swift_task_getJobTaskId")
internal func _getJobTaskId(_ job: UnownedJob) -> UInt64

@available(SwiftStdlib 5.9, *)
@_silgen_name("_task_serialExecutor_isSameExclusiveExecutionContext")
internal func _task_serialExecutor_isSameExclusiveExecutionContext<E>(current currentExecutor: E, executor: E) -> Bool
    where E: SerialExecutor {
  currentExecutor.isSameExclusiveExecutionContext(other: executor)
}

@available(SwiftStdlib 6.0, *)
@_silgen_name("_task_serialExecutor_checkIsolated")
internal func _task_serialExecutor_checkIsolated<E>(executor: E)
    where E: SerialExecutor {
  executor.checkIsolated()
}

@available(SwiftStdlib 6.2, *)
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
@available(SwiftStdlib 5.9, *)
@_silgen_name("_task_serialExecutor_getExecutorRef")
internal func _task_serialExecutor_getExecutorRef<E>(_ executor: E) -> Builtin.Executor
    where E: SerialExecutor {
  return unsafe executor.asUnownedSerialExecutor().executor
}

/// Obtain the executor ref by calling the executor's `asUnownedTaskExecutor()`.
/// The obtained executor ref will have all the user-defined flags set on the executor.
@_unavailableInEmbedded
@available(SwiftStdlib 6.0, *)
@_silgen_name("_task_taskExecutor_getTaskExecutorRef")
internal func _task_taskExecutor_getTaskExecutorRef<E>(_ taskExecutor: E) -> Builtin.Executor
    where E: TaskExecutor {
  return taskExecutor.asUnownedTaskExecutor().executor
}

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
