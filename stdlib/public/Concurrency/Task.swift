//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift
@_implementationOnly import SwiftConcurrencyInternalShims

// ==== Task -------------------------------------------------------------------

/// A unit of asynchronous work.
///
/// When you create an instance of `Task`,
/// you provide a closure that contains the work for that task to perform.
/// Tasks can start running immediately after creation;
/// you don't explicitly start or schedule them.
/// After creating a task, you use the instance to interact with it ---
/// for example, to wait for it to complete or to cancel it.
/// It's not a programming error to discard a reference to a task
/// without waiting for that task to finish or canceling it.
/// A task runs regardless of whether you keep a reference to it.
/// However, if you discard the reference to a task,
/// you give up the ability
/// to wait for that task's result or cancel the task.
///
/// To support operations on the current task,
/// which can be either a detached task or child task,
/// `Task` also exposes class methods like `yield()`.
/// Because these methods are asynchronous,
/// they're always invoked as part of an existing task.
///
/// Only code that's running as part of the task can interact with that task.
/// To interact with the current task,
/// you call one of the static methods on `Task`.
///
/// A task's execution can be seen as a series of periods where the task ran.
/// Each such period ends at a suspension point or the
/// completion of the task.
/// These periods of execution are represented by instances of `PartialAsyncTask`.
/// Unless you're implementing a custom executor,
/// you don't directly interact with partial tasks.
///
/// For information about the language-level concurrency model that `Task` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
/// [tspl]: https://docs.swift.org/swift-book/
///
/// Task Cancellation
/// =================
///
/// Tasks include a shared mechanism for indicating cancellation,
/// but not a shared implementation for how to handle cancellation.
/// Depending on the work you're doing in the task,
/// the correct way to stop that work varies.
/// Likewise,
/// it's the responsibility of the code running as part of the task
/// to check for cancellation whenever stopping is appropriate.
/// In a long-task that includes multiple pieces,
/// you might need to check for cancellation at several points,
/// and handle cancellation differently at each point.
/// If you only need to throw an error to stop the work,
/// call the `Task.checkCancellation()` function to check for cancellation.
/// Other responses to cancellation include
/// returning the work completed so far, returning an empty result, or returning `nil`.
///
/// Cancellation is a purely Boolean state;
/// there's no way to include additional information
/// like the reason for cancellation.
/// This reflects the fact that a task can be canceled for many reasons,
/// and additional reasons can accrue during the cancellation process.
///
/// ### Task closure lifetime
/// Tasks are initialized by passing a closure containing the code that will be executed by a given task.
///
/// After this code has run to completion, the task has completed, resulting in either
/// a failure or result value, this closure is eagerly released.
///
/// Retaining a task object doesn't indefinitely retain the closure,
/// because any references that a task holds are released
/// after the task completes.
/// Consequently, tasks rarely need to capture weak references to values.
///
/// For example, in the following snippet of code it is not necessary to capture the actor as `weak`,
/// because as the task completes it'll let go of the actor reference, breaking the
/// reference cycle between the Task and the actor holding it.
///
/// ```
/// struct Work: Sendable {}
///
/// actor Worker {
///     var work: Task<Void, Never>?
///     var result: Work?
///
///     deinit {
///         // even though the task is still retained,
///         // once it completes it no longer causes a reference cycle with the actor
///
///         print("deinit actor")
///     }
///
///     func start() {
///         work = Task {
///             print("start task work")
///             try? await Task.sleep(for: .seconds(3))
///             self.result = Work() // we captured self
///             print("completed task work")
///             // but as the task completes, this reference is released
///         }
///         // we keep a strong reference to the task
///     }
/// }
/// ```
///
/// And using it like this:
///
/// ```
/// await Worker().start()
/// ```
///
/// Note that the actor is only retained by the start() method's use of `self`,
/// and that the start method immediately returns, without waiting for the
/// unstructured `Task` to finish. Once the task is completed and its closure is
/// destroyed, the strong reference to the actor is also released allowing the
/// actor to deinitialize as expected.
///
/// Therefore, the above call will consistently result in the following output:
///
/// ```other
/// start task work
/// completed task work
/// deinit actor
/// ```
@available(SwiftStdlib 5.1, *)
@frozen
public struct Task<Success: Sendable, Failure: Error>: Sendable {
  @usableFromInline
  internal let _task: Builtin.NativeObject

  @_alwaysEmitIntoClient
  internal init(_ task: Builtin.NativeObject) {
    self._task = task
  }
}

@available(SwiftStdlib 5.1, *)
extension Task {
  /// The result from a throwing task, after it completes.
  ///
  /// If the task hasn't completed,
  /// accessing this property waits for it to complete
  /// and its priority increases to that of the current task.
  /// Note that this might not be as effective as
  /// creating the task with the correct priority,
  /// depending on the executor's scheduling details.
  ///
  /// If the task throws an error, this property propagates that error.
  /// Tasks that respond to cancellation by throwing `CancellationError`
  /// have that error propagated here upon cancellation.
  ///
  /// - Returns: The task's result.
  public var value: Success {
    get async throws {
      return try await _taskFutureGetThrowing(_task)
    }
  }

  /// The result or error from a throwing task, after it completes.
  ///
  /// If the task hasn't completed,
  /// accessing this property waits for it to complete
  /// and its priority increases to that of the current task.
  /// Note that this might not be as effective as
  /// creating the task with the correct priority,
  /// depending on the executor's scheduling details.
  ///
  /// - Returns: If the task succeeded,
  ///   `.success` with the task's result as the associated value;
  ///   otherwise, `.failure` with the error as the associated value.
  public var result: Result<Success, Failure> {
    get async {
      do {
        return .success(try await value)
      } catch {
        return .failure(error as! Failure) // as!-safe, guaranteed to be Failure
      }
    }
  }

  /// Cancels this task.
  ///
  /// Cancelling a task has three primary effects:
  ///
  /// - It flags the task as canceled.
  /// - It causes any active cancellation handlers on the task to run, once.
  /// - It cancels any child tasks and task groups of the task, including
  ///   those created in the future. If those tasks have cancellation handlers,
  ///   they also are triggered.
  ///
  /// Task cancellation is cooperative and idempotent.
  ///
  /// Cancelling a task does not automatically cause arbitrary functions on the task
  /// to stop running or throw errors. A function _may_ choose to react
  /// to cancellation by ending its work early, and it is conventional to
  /// signal that to callers by throwing `CancellationError`. However,
  /// a function that doesn't specifically check for cancellation will
  /// run to completion normally, even if the task it is running on is
  /// canceled. However, that function might still end early if it calls
  /// other code that handles cancellation by throwing and that function doesn't
  /// handle the error.
  ///
  /// It's safe to cancel a task from any task or thread. It's safe for
  /// multiple tasks or threads to cancel the same task at the same
  /// time. Cancelling a task that has already been canceled has no
  /// additional effect.
  ///
  /// `cancel` may need to acquire locks and synchronously run
  /// arbitrary cancellation-handler code associated with the
  /// canceled task. To reduce the risk of deadlock, it is
  /// recommended that callers release any locks they might be
  /// holding before they call cancel.
  ///
  /// If the task has already run past the last point where it could have 
  /// performed a cancellation check, cancelling it may have no observable effects.
  ///
  /// - SeeAlso: `Task.checkCancellation()`
  /// - SeeAlso: `withTaskCancellationHandler(operation:onCancel:isolation:)`
  public func cancel() {
    Builtin.cancelAsyncTask(_task)
  }
}

@available(SwiftStdlib 5.1, *)
extension Task where Failure == Never {
  /// The result from a nonthrowing task, after it completes.
  ///
  /// If the task hasn't completed yet,
  /// accessing this property waits for it to complete
  /// and its priority increases to that of the current task.
  /// Note that this might not be as effective as
  /// creating the task with the correct priority,
  /// depending on the executor's scheduling details.
  ///
  /// Tasks that never throw an error can still check for cancellation,
  /// but they need to use an approach like returning `nil`
  /// instead of throwing an error.
  public var value: Success {
    get async {
      return await _taskFutureGet(_task)
    }
  }
}

@available(SwiftStdlib 5.1, *)
extension Task: Hashable {
  public func hash(into hasher: inout Hasher) {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(_task)).hash(into: &hasher)
  }
}

@available(SwiftStdlib 5.1, *)
extension Task: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(lhs._task)) ==
      UnsafeRawPointer(Builtin.bridgeToRawPointer(rhs._task))
  }
}

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY && !SWIFT_CONCURRENCY_EMBEDDED
@available(SwiftStdlib 5.9, *)
extension Task where Failure == Error {
    @_spi(MainActorUtilities)
    @MainActor
    @available(SwiftStdlib 5.9, *)
    @discardableResult
    public static func startOnMainActor(
        priority: TaskPriority? = nil,
        @_inheritActorContext @_implicitSelfCapture _ work: __owned @Sendable @escaping @MainActor() async throws -> Success
    ) -> Task<Success, Error> {
        let flags = taskCreateFlags(priority: priority, isChildTask: false,
                                    copyTaskLocals: true, inheritContext: true,
                                    enqueueJob: false,
                                    addPendingGroupTaskUnconditionally: false,
                                    isDiscardingTask: false)
        let (task, _) = Builtin.createAsyncTask(flags, work)
        _startTaskOnMainActor(task)
        return Task<Success, Error>(task)
    }
}
#endif

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY && !SWIFT_CONCURRENCY_EMBEDDED
@available(SwiftStdlib 5.9, *)
extension Task where Failure == Never {
    @_spi(MainActorUtilities)
    @MainActor
    @available(SwiftStdlib 5.9, *)
    @discardableResult
    public static func startOnMainActor(
        priority: TaskPriority? = nil,
        @_inheritActorContext @_implicitSelfCapture _ work: __owned @Sendable @escaping @MainActor() async -> Success
    ) -> Task<Success, Never> {
        let flags = taskCreateFlags(priority: priority, isChildTask: false,
                                    copyTaskLocals: true, inheritContext: true,
                                    enqueueJob: false,
                                    addPendingGroupTaskUnconditionally: false,
                                    isDiscardingTask: false)
        let (task, _) = Builtin.createAsyncTask(flags, work)
        _startTaskOnMainActor(task)
        return Task(task)
    }
}
#endif

// ==== Task Priority ----------------------------------------------------------

/// The priority of a task.
///
/// The executor determines how priority information affects the way tasks are scheduled.
/// The behavior varies depending on the executor currently being used.
/// Typically, executors attempt to run tasks with a higher priority
/// before tasks with a lower priority.
/// However, the semantics of how priority is treated are left up to each
/// platform and `Executor` implementation.
///
/// Child tasks automatically inherit their parent task's priority.
/// Detached tasks created by `detach(priority:operation:)` don't inherit task priority
/// because they aren't attached to the current task.
///
/// In some situations the priority of a task is elevated ---
/// that is, the task is treated as it if had a higher priority,
/// without actually changing the priority of the task:
///
/// - If a task runs on behalf of an actor,
///   and a new higher-priority task is enqueued to the actor,
///   then the actor's current task is temporarily elevated
///   to the priority of the enqueued task.
///   This priority elevation allows the new task
///   to be processed at the priority it was enqueued with.
/// - If a a higher-priority task calls the `get()` method,
///   then the priority of this task increases until the task completes.
///
/// In both cases, priority elevation helps you prevent a low-priority task
/// from blocking the execution of a high priority task,
/// which is also known as *priority inversion*.
@available(SwiftStdlib 5.1, *)
public struct TaskPriority: RawRepresentable, Sendable {
  public typealias RawValue = UInt8
  public var rawValue: UInt8

  public init(rawValue: UInt8) {
    self.rawValue = rawValue
  }

  public static let high: TaskPriority = .init(rawValue: 0x19)

  @_alwaysEmitIntoClient
  public static var medium: TaskPriority {
    .init(rawValue: 0x15)
  }

  public static let low: TaskPriority = .init(rawValue: 0x11)

  public static let userInitiated: TaskPriority = high
  public static let utility: TaskPriority = low
  public static let background: TaskPriority = .init(rawValue: 0x09)

  @available(*, deprecated, renamed: "medium")
  public static let `default`: TaskPriority = .init(rawValue: 0x15)
}

@available(SwiftStdlib 5.1, *)
extension TaskPriority: Equatable {
  public static func == (lhs: TaskPriority, rhs: TaskPriority) -> Bool {
    lhs.rawValue == rhs.rawValue
  }

  public static func != (lhs: TaskPriority, rhs: TaskPriority) -> Bool {
    lhs.rawValue != rhs.rawValue
  }
}

@available(SwiftStdlib 5.1, *)
extension TaskPriority: Comparable {
  public static func < (lhs: TaskPriority, rhs: TaskPriority) -> Bool {
    lhs.rawValue < rhs.rawValue
  }

  public static func <= (lhs: TaskPriority, rhs: TaskPriority) -> Bool {
    lhs.rawValue <= rhs.rawValue
  }

  public static func > (lhs: TaskPriority, rhs: TaskPriority) -> Bool {
    lhs.rawValue > rhs.rawValue
  }

  public static func >= (lhs: TaskPriority, rhs: TaskPriority) -> Bool {
    lhs.rawValue >= rhs.rawValue
  }
}


@available(SwiftStdlib 5.9, *)
@_unavailableInEmbedded
extension TaskPriority: CustomStringConvertible {
  @available(SwiftStdlib 5.9, *)
  public var description: String {
    switch self.rawValue {
    case Self.low.rawValue:
      return "\(Self.self).low"
    case Self.medium.rawValue:
      return "\(Self.self).medium"
    case Self.high.rawValue:
      return "\(Self.self).high"
    case Self.background.rawValue:
      return "\(Self.self).background"
    default:
      return "\(Self.self)(rawValue: \(self.rawValue))"
    }
  }
}

#if !SWIFT_CONCURRENCY_EMBEDDED
@available(SwiftStdlib 5.1, *)
extension TaskPriority: Codable { }
#endif

@available(SwiftStdlib 5.1, *)
extension Task where Success == Never, Failure == Never {

  /// The current task's priority.
  ///
  /// If you access this property outside of any task,
  /// this queries the system to determine the
  /// priority at which the current function is running.
  /// If the system can't provide a priority,
  /// this property's value is `Priority.default`.
  public static var currentPriority: TaskPriority {
    withUnsafeCurrentTask { unsafeTask in
      // If we are running on behalf of a task, use that task's priority.
      if let unsafeTask {
         return unsafeTask.priority
      }

      // Otherwise, query the system.
      return TaskPriority(rawValue: UInt8(_getCurrentThreadPriority()))
    }
  }

  /// The current task's base priority.
  ///
  /// If you access this property outside of any task, this returns nil
  @available(SwiftStdlib 5.7, *)
  public static var basePriority: TaskPriority? {
    withUnsafeCurrentTask { task in
      // If we are running on behalf of a task, use that task's priority.
      if let unsafeTask = task {
         return TaskPriority(rawValue: _taskBasePriority(unsafeTask._task))
      }
      return nil
    }
  }

}

@available(SwiftStdlib 5.1, *)
extension TaskPriority {
  /// Downgrade user-interactive to user-initiated.
  var _downgradeUserInteractive: TaskPriority {
    return self
  }
}

// ==== Job Flags --------------------------------------------------------------

/// Flags for schedulable jobs.
///
/// This is a port of the C++ FlagSet.
@available(SwiftStdlib 5.1, *)
struct JobFlags {
  /// Kinds of schedulable jobs.
  enum Kind: Int32 {
    case task = 0
  }

  /// The actual bit representation of these flags.
  var bits: Int32 = 0

  /// The kind of job described by these flags.
  var kind: Kind {
    get {
      Kind(rawValue: bits & 0xFF)!
    }

    set {
      bits = (bits & ~0xFF) | newValue.rawValue
    }
  }

  /// Whether this is an asynchronous task.
  var isAsyncTask: Bool { kind == .task }

  /// The priority given to the job.
  var priority: TaskPriority? {
    get {
      let value = (Int(bits) & 0xFF00) >> 8

      if value == 0 {
        return nil
      }

      return TaskPriority(rawValue: UInt8(value))
    }

    set {
      bits = (bits & ~0xFF00) | Int32((Int(newValue?.rawValue ?? 0) << 8))
    }
  }

  /// Whether this is a child task.
  var isChildTask: Bool {
    get {
      (bits & (1 << 24)) != 0
    }

    set {
      if newValue {
        bits = bits | 1 << 24
      } else {
        bits = (bits & ~(1 << 24))
      }
    }
  }

  /// Whether this is a future.
  var isFuture: Bool {
    get {
      (bits & (1 << 25)) != 0
    }

    set {
      if newValue {
        bits = bits | 1 << 25
      } else {
        bits = (bits & ~(1 << 25))
      }
    }
  }

  /// Whether this is a group child.
  var isGroupChildTask: Bool {
    get {
      (bits & (1 << 26)) != 0
    }

    set {
      if newValue {
        bits = bits | 1 << 26
      } else {
        bits = (bits & ~(1 << 26))
      }
    }
  }

  /// Whether this is a task created by the 'async' operation, which
  /// conceptually continues the work of the synchronous code that invokes
  /// it.
  var isContinuingAsyncTask: Bool {
    get {
      (bits & (1 << 27)) != 0
    }

    set {
      if newValue {
        bits = bits | 1 << 27
      } else {
        bits = (bits & ~(1 << 27))
      }
    }
  }
}

// ==== Task Creation Flags --------------------------------------------------

/// Form task creation flags for use with the createAsyncTask builtins.
@available(SwiftStdlib 5.1, *)
@_alwaysEmitIntoClient
func taskCreateFlags(
  priority: TaskPriority?, isChildTask: Bool, copyTaskLocals: Bool,
  inheritContext: Bool, enqueueJob: Bool,
  addPendingGroupTaskUnconditionally: Bool,
  isDiscardingTask: Bool
) -> Int {
  var bits = 0
  bits |= (bits & ~0xFF) | Int(priority?.rawValue ?? 0)
  if isChildTask {
    bits |= 1 << 8
  }
  if copyTaskLocals {
    bits |= 1 << 10
  }
  if inheritContext {
    bits |= 1 << 11
  }
  if enqueueJob {
    bits |= 1 << 12
  }
  if addPendingGroupTaskUnconditionally {
    bits |= 1 << 13
  }
  if isDiscardingTask {
    bits |= 1 << 14
  }
  return bits
}

// ==== Task Creation ----------------------------------------------------------

@available(SwiftStdlib 5.1, *)
extension Task where Failure == Never {
#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public init(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async -> Success
  ) {
    fatalError("Unavailable in task-to-thread concurrency model.")
  }
#else
  /// Runs the given nonthrowing operation asynchronously
  /// as part of a new top-level task on behalf of the current actor.
  ///
  /// Use this function when creating asynchronous work
  /// that operates on behalf of the synchronous function that calls it.
  /// Like `Task.detached(priority:operation:)`,
  /// this function creates a separate, top-level task.
  /// Unlike `Task.detached(priority:operation:)`,
  /// the task created by `Task.init(priority:operation:)`
  /// inherits the priority and actor context of the caller,
  /// so the operation is treated more like an asynchronous extension
  /// to the synchronous operation.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a detached task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - priority: The priority of the task.
  ///     Pass `nil` to use the priority from `Task.currentPriority`.
  ///   - operation: The operation to perform.
  @discardableResult
  @_alwaysEmitIntoClient
  public init(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async -> Success
  ) {
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task.
    let builtinSerialExecutor =
      Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    let (task, _) = Builtin.createTask(flags: flags,
                                       initialSerialExecutor:
                                         builtinSerialExecutor,
                                       operation: operation)

    self._task = task
  }
#endif
}

@available(SwiftStdlib 5.1, *)
extension Task where Failure == Error {
#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public init(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async throws -> Success
  ) {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
#else
  /// Runs the given throwing operation asynchronously
  /// as part of a new top-level task on behalf of the current actor.
  ///
  /// Use this function when creating asynchronous work
  /// that operates on behalf of the synchronous function that calls it.
  /// Like `Task.detached(priority:operation:)`,
  /// this function creates a separate, top-level task.
  /// Unlike `detach(priority:operation:)`,
  /// the task created by `Task.init(priority:operation:)`
  /// inherits the priority and actor context of the caller,
  /// so the operation is treated more like an asynchronous extension
  /// to the synchronous operation.
  ///
  /// You need to keep a reference to the task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a detached task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - priority: The priority of the task.
  ///     Pass `nil` to use the priority from `Task.currentPriority`.
  ///   - operation: The operation to perform.
  @discardableResult
  @_alwaysEmitIntoClient
  public init(
    priority: TaskPriority? = nil,
    @_inheritActorContext @_implicitSelfCapture operation: sending @escaping @isolated(any) () async throws -> Success
  ) {
    // Set up the task flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: true,
      inheritContext: true, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task future.
    let builtinSerialExecutor =
      Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    let (task, _) = Builtin.createTask(flags: flags,
                                       initialSerialExecutor:
                                         builtinSerialExecutor,
                                       operation: operation)

    self._task = task
  }
#endif
}

// ==== Detached Tasks ---------------------------------------------------------

@available(SwiftStdlib 5.1, *)
extension Task where Failure == Never {
#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func detached(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> Success
  ) -> Task<Success, Failure> {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
#else
  /// Runs the given nonthrowing operation asynchronously
  /// as part of a new top-level task.
  ///
  /// Don't use a detached task if it's possible
  /// to model the operation using structured concurrency features like child tasks.
  /// Child tasks inherit the parent task's priority and task-local storage,
  /// and canceling a parent task automatically cancels all of its child tasks.
  /// You need to handle these considerations manually with a detached task.
  ///
  /// You need to keep a reference to the detached task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a detached task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - priority: The priority of the task.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @discardableResult
  @_alwaysEmitIntoClient
  public static func detached(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async -> Success
  ) -> Task<Success, Failure> {
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task future.
    let builtinSerialExecutor =
      Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    let (task, _) = Builtin.createTask(flags: flags,
                                       initialSerialExecutor:
                                         builtinSerialExecutor,
                                       operation: operation)

    return Task(task)
  }
#endif
}

@available(SwiftStdlib 5.1, *)
extension Task where Failure == Error {
#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  @discardableResult
  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func detached(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws -> Success
  ) -> Task<Success, Failure> {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
#else
  /// Runs the given throwing operation asynchronously
  /// as part of a new top-level task.
  ///
  /// If the operation throws an error, this method propagates that error.
  ///
  /// Don't use a detached task if it's possible
  /// to model the operation using structured concurrency features like child tasks.
  /// Child tasks inherit the parent task's priority and task-local storage,
  /// and canceling a parent task automatically cancels all of its child tasks.
  /// You need to handle these considerations manually with a detached task.
  ///
  /// You need to keep a reference to the detached task
  /// if you want to cancel it by calling the `Task.cancel()` method.
  /// Discarding your reference to a detached task
  /// doesn't implicitly cancel that task,
  /// it only makes it impossible for you to explicitly cancel the task.
  ///
  /// - Parameters:
  ///   - priority: The priority of the task.
  ///   - operation: The operation to perform.
  ///
  /// - Returns: A reference to the task.
  @discardableResult
  @_alwaysEmitIntoClient
  public static func detached(
    priority: TaskPriority? = nil,
    operation: sending @escaping @isolated(any) () async throws -> Success
  ) -> Task<Success, Failure> {
    // Set up the job flags for a new task.
    let flags = taskCreateFlags(
      priority: priority, isChildTask: false, copyTaskLocals: false,
      inheritContext: false, enqueueJob: true,
      addPendingGroupTaskUnconditionally: false,
      isDiscardingTask: false)

    // Create the asynchronous task future.
    let builtinSerialExecutor =
      Builtin.extractFunctionIsolation(operation)?.unownedExecutor.executor

    let (task, _) = Builtin.createTask(flags: flags,
                                       initialSerialExecutor:
                                         builtinSerialExecutor,
                                       operation: operation)

    return Task(task)
  }
#endif
}

// ==== Voluntary Suspension -----------------------------------------------------

@available(SwiftStdlib 5.1, *)
extension Task where Success == Never, Failure == Never {

  /// Suspends the current task and allows other tasks to execute.
  ///
  /// A task can voluntarily suspend itself
  /// in the middle of a long-running operation
  /// that doesn't contain any suspension points,
  /// to let other tasks run for a while
  /// before execution returns to this task.
  ///
  /// If this task is the highest-priority task in the system,
  /// the executor immediately resumes execution of the same task.
  /// As such,
  /// this method isn't necessarily a way to avoid resource starvation.
  public static func yield() async {
    return await Builtin.withUnsafeContinuation { (continuation: Builtin.RawUnsafeContinuation) -> Void in
      let job = _taskCreateNullaryContinuationJob(
          priority: Int(Task.currentPriority.rawValue),
          continuation: continuation)
      _enqueueJobGlobal(job)
    }
  }
}

// ==== UnsafeCurrentTask ------------------------------------------------------

/// Calls a closure with an unsafe reference to the current task.
///
/// If you call this function from the body of an asynchronous function,
/// the unsafe task handle passed to the closure is always non-`nil`
/// because an asynchronous function always runs in the context of a task.
/// However, if you call this function from the body of a synchronous function,
/// and that function isn't executing in the context of any task,
/// the unsafe task handle is `nil`.
///
/// Don't store an unsafe task reference
/// for use outside this method's closure.
/// Storing an unsafe reference doesn't affect the task's actual life cycle,
/// and the behavior of accessing an unsafe task reference
/// outside of the `withUnsafeCurrentTask(body:)` method's closure isn't defined.
/// There's no safe way to retrieve a reference to the current task
/// and save it for long-term use.
/// To query the current task without saving a reference to it,
/// use properties like `currentPriority`.
/// If you need to store a reference to a task,
/// create an unstructured task using `Task.detached(priority:operation:)` instead.
///
/// - Parameters:
///   - body: A closure that takes an `UnsafeCurrentTask` parameter.
///     If `body` has a return value,
///     that value is also used as the return value
///     for the `withUnsafeCurrentTask(body:)` function.
///
/// - Returns: The return value, if any, of the `body` closure.
@available(SwiftStdlib 5.1, *)
public func withUnsafeCurrentTask<T>(body: (UnsafeCurrentTask?) throws -> T) rethrows -> T {
  guard let _task = _getCurrentAsyncTask() else {
    return try body(nil)
  }

  // FIXME: This retain seems pretty wrong, however if we don't we WILL crash
  //        with "destroying a task that never completed" in the task's destroy.
  //        How do we solve this properly?
  Builtin.retain(_task)

  return try body(UnsafeCurrentTask(_task))
}

@available(SwiftStdlib 6.0, *)
public func withUnsafeCurrentTask<T>(body: (UnsafeCurrentTask?) async throws -> T) async rethrows -> T {
  guard let _task = _getCurrentAsyncTask() else {
    return try await body(nil)
  }

  // FIXME: This retain seems pretty wrong, however if we don't we WILL crash
  //        with "destroying a task that never completed" in the task's destroy.
  //        How do we solve this properly?
  Builtin.retain(_task)

  return try await body(UnsafeCurrentTask(_task))
}

/// An unsafe reference to the current task.
///
/// To get an instance of `UnsafeCurrentTask` for the current task,
/// call the `withUnsafeCurrentTask(body:)` method.
/// Don't store an unsafe task reference
/// for use outside that method's closure.
/// Storing an unsafe reference doesn't affect the task's actual life cycle,
/// and the behavior of accessing an unsafe task reference
/// outside of the `withUnsafeCurrentTask(body:)` method's closure isn't defined.
///
/// Only APIs on `UnsafeCurrentTask` that are also part of `Task`
/// are safe to invoke from a task other than
/// the task that this `UnsafeCurrentTask` instance refers to.
/// Calling other APIs from another task is undefined behavior,
/// breaks invariants in other parts of the program running on this task,
/// and may lead to crashes or data loss.
///
/// For information about the language-level concurrency model that `UnsafeCurrentTask` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/LanguageGuide/Concurrency.html
/// [tspl]: https://docs.swift.org/swift-book/
@available(SwiftStdlib 5.1, *)
@unsafe
public struct UnsafeCurrentTask {
  internal let _task: Builtin.NativeObject

  // May only be created by the standard library.
  internal init(_ task: Builtin.NativeObject) {
    self._task = task
  }

  /// A Boolean value that indicates whether the current task was canceled.
  ///
  /// After the value of this property becomes `true`, it remains `true` indefinitely.
  /// There is no way to uncancel a task.
  ///
  /// - SeeAlso: `checkCancellation()`
  public var isCancelled: Bool {
    _taskIsCancelled(_task)
  }

  /// The current task's priority.
  ///
  /// - SeeAlso: `TaskPriority`
  /// - SeeAlso: `Task.currentPriority`
  public var priority: TaskPriority {
    TaskPriority(rawValue: _taskCurrentPriority(_task))
  }

  /// The current task's base priority.
  ///
  /// - SeeAlso: `TaskPriority`
  /// - SeeAlso: `Task.basePriority`
  @available(SwiftStdlib 5.9, *)
  public var basePriority: TaskPriority {
    TaskPriority(rawValue: _taskBasePriority(_task))
  }

  /// Cancel the current task.
  public func cancel() {
    _taskCancel(_task)
  }
}

@available(SwiftStdlib 5.1, *)
@available(*, unavailable)
extension UnsafeCurrentTask: Sendable { }

@available(SwiftStdlib 5.1, *)
extension UnsafeCurrentTask: Hashable {
  public func hash(into hasher: inout Hasher) {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(_task)).hash(into: &hasher)
  }
}

@available(SwiftStdlib 5.1, *)
extension UnsafeCurrentTask: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    UnsafeRawPointer(Builtin.bridgeToRawPointer(lhs._task)) ==
      UnsafeRawPointer(Builtin.bridgeToRawPointer(rhs._task))
  }
}

// ==== Internal ---------------------------------------------------------------
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_getCurrent")
public func _getCurrentAsyncTask() -> Builtin.NativeObject?

@_silgen_name("swift_task_startOnMainActor")
fileprivate func _startTaskOnMainActor(_ task: Builtin.NativeObject)

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_getJobFlags")
func getJobFlags(_ task: Builtin.NativeObject) -> JobFlags

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_enqueueGlobal")
@usableFromInline
func _enqueueJobGlobal(_ task: Builtin.Job)

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_enqueueGlobalWithDelay")
@usableFromInline
func _enqueueJobGlobalWithDelay(_ delay: UInt64, _ task: Builtin.Job)

@available(SwiftStdlib 5.7, *)
@_silgen_name("swift_task_enqueueGlobalWithDeadline")
@usableFromInline
func _enqueueJobGlobalWithDeadline(_ seconds: Int64, _ nanoseconds: Int64,
                                   _ toleranceSec: Int64, _ toleranceNSec: Int64,
                                   _ clock: Int32, _ task: Builtin.Job)

@available(SwiftStdlib 5.1, *)
@usableFromInline
@_silgen_name("swift_task_asyncMainDrainQueue")
internal func _asyncMainDrainQueue() -> Never

@available(SwiftStdlib 5.1, *)
@usableFromInline
@_silgen_name("swift_task_getMainExecutor")
internal func _getMainExecutor() -> Builtin.Executor

/// Get the default generic serial executor.
///
/// It is used by default used by tasks and default actors,
/// unless other executors are specified.
@available(SwiftStdlib 6.0, *)
@usableFromInline
internal func _getGenericSerialExecutor() -> Builtin.Executor {
  // The `SerialExecutorRef` to "default generic executor" is guaranteed
  // to be a zero value;
  //
  // As the runtime relies on this in multiple places,
  // so instead of a runtime call to get this executor ref, we bitcast a "zero"
  // of expected size to the builtin executor type.
  unsafeBitCast((UInt(0), UInt(0)), to: Builtin.Executor.self)
}

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 5.1, *)
@available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
@usableFromInline
@preconcurrency
internal func _runAsyncMain(_ asyncFun: @Sendable @escaping () async throws -> ()) {
  fatalError("Unavailable in task-to-thread concurrency model")
}
#elseif !SWIFT_CONCURRENCY_EMBEDDED
@available(SwiftStdlib 5.1, *)
@usableFromInline
@preconcurrency
internal func _runAsyncMain(_ asyncFun: @Sendable @escaping () async throws -> ()) {
  Task.detached {
    do {
#if !os(Windows)
#if compiler(>=5.5) && $BuiltinHopToActor
      Builtin.hopToActor(MainActor.shared)
#else
      fatalError("Swift compiler is incompatible with this SDK version")
#endif
#endif
      try await asyncFun()
      exit(0)
    } catch {
      _errorInMain(error)
    }
  }
  _asyncMainDrainQueue()
}
#endif

// FIXME: both of these ought to take their arguments _owned so that
//        we can do a move out of the future in the common case where it's
//        unreferenced
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_future_wait")
public func _taskFutureGet<T>(_ task: Builtin.NativeObject) async -> T

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_future_wait_throwing")
public func _taskFutureGetThrowing<T>(_ task: Builtin.NativeObject) async throws -> T

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_cancel")
public func _taskCancel(_ task: Builtin.NativeObject)

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_isCancelled")
@usableFromInline
func _taskIsCancelled(_ task: Builtin.NativeObject) -> Bool

@_silgen_name("swift_task_currentPriority")
internal func _taskCurrentPriority(_ task: Builtin.NativeObject) -> UInt8

@_silgen_name("swift_task_basePriority")
internal func _taskBasePriority(_ task: Builtin.NativeObject) -> UInt8

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_createNullaryContinuationJob")
func _taskCreateNullaryContinuationJob(priority: Int, continuation: Builtin.RawUnsafeContinuation) -> Builtin.Job

@available(SwiftStdlib 5.1, *)
@usableFromInline
@_silgen_name("swift_task_isCurrentExecutor")
func _taskIsCurrentExecutor(_ executor: Builtin.Executor) -> Bool

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY && !SWIFT_CONCURRENCY_EMBEDDED

@available(SwiftStdlib 9999, *)
@_silgen_name("swift_task_isCurrentExecutorWithFlags")
@usableFromInline
internal func _taskIsCurrentExecutor(
  executor: Builtin.Executor, flags: UInt64) -> Bool

#endif

@available(SwiftStdlib 5.1, *)
@usableFromInline
@_silgen_name("swift_task_reportUnexpectedExecutor")
func _reportUnexpectedExecutor(_ _filenameStart: Builtin.RawPointer,
                               _ _filenameLength: Builtin.Word,
                               _ _filenameIsASCII: Builtin.Int1,
                               _ _line: Builtin.Word,
                               _ _executor: Builtin.Executor)

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_getCurrentThreadPriority")
func _getCurrentThreadPriority() -> Int

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 5.8, *)
@usableFromInline
@_unavailableFromAsync(message: "Use _taskRunInline from a sync context to begin an async context.")
internal func _taskRunInline<T>(_ body: () async -> T) -> T {
#if compiler(>=5.5) && $BuiltinTaskRunInline
  return Builtin.taskRunInline(body)
#else
  fatalError("Unsupported Swift compiler")
#endif
}

@available(SwiftStdlib 5.8, *)
extension Task where Failure == Never {
  /// Start an async context within the current sync context and run the
  /// provided async closure, returning the value it produces.
  @available(SwiftStdlib 5.8, *)
  @_spi(_TaskToThreadModel)
  @_unavailableFromAsync(message: "Use Task.runInline from a sync context to begin an async context.")
  public static func runInline(_ body: () async -> Success) -> Success {
    return _taskRunInline(body)
  }
}

@available(SwiftStdlib 5.8, *)
extension Task where Failure == Error {
  @available(SwiftStdlib 5.8, *)
  @_alwaysEmitIntoClient
  @usableFromInline
  internal static func _runInlineHelper<T>(
    body: () async -> Result<T, Error>,
    rescue: (Result<T, Error>) throws -> T
  ) rethrows -> T {
    return try rescue(
      _taskRunInline(body)
    )
  }

  /// Start an async context within the current sync context and run the
  /// provided async closure, returning or throwing the value or error it
  /// produces.
  @available(SwiftStdlib 5.8, *)
  @_spi(_TaskToThreadModel)
  @_unavailableFromAsync(message: "Use Task.runInline from a sync context to begin an async context.")
  public static func runInline(_ body: () async throws -> Success) rethrows -> Success {
    return try _runInlineHelper(
      body: {
        do {
          let value = try await body()
          return Result.success(value)
        }
        catch let error {
          return Result.failure(error)
        }
    },
      rescue: { try $0.get() }
    )
  }
}
#endif

#if _runtime(_ObjC)

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
/// Intrinsic used by SILGen to launch a task for bridging a Swift async method
/// which was called through its ObjC-exported completion-handler-based API.
@available(SwiftStdlib 5.1, *)
@_alwaysEmitIntoClient
@usableFromInline
internal func _runTaskForBridgedAsyncMethod(@_inheritActorContext _ body: __owned @Sendable @escaping () async -> Void) {
#if compiler(>=5.6)
  Task(operation: body)
#else
  Task<Int, Error> {
    await body()
    return 0
  }
#endif
}
#endif

#endif

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY && !SWIFT_CONCURRENCY_EMBEDDED

@available(SwiftStdlib 9999, *)
@_alwaysEmitIntoClient
@usableFromInline
internal func _taskIsOnMainActor() -> Bool {
  // 0 for check mode means do not assert.
  return _taskIsCurrentExecutor(executor: _getMainExecutor(),
                                flags: 0)
}

/// SPI that is used by the compiler to implement the hop to main actor if
/// needed thunk. This thunk is used when passing preconcurrency code a main
/// actor isolated callback. If when invoked, we are dynamically on the main
/// actor, we just invoke the function without creating a task. Otherwise, we
/// create a Task and run operation within it.
///
/// NOTE: For this to be safe, we need our function to not have any return value
/// and that includes an error return value. So we cannot accept throwing
/// functions here.
@_alwaysEmitIntoClient
@available(SwiftStdlib 9999, *)
public func _taskRunOnMainActor(operation: @escaping @MainActor () -> ()) {
  typealias YesActor = @MainActor () -> ()
  typealias NoActor = () -> ()

  // First check if we are on the main actor. If so, just call the synchronous
  // function directly.
  if _taskIsOnMainActor() {
    return withoutActuallyEscaping(operation) {
      (_ fn: @escaping YesActor) -> () in
      let rawFn = unsafeBitCast(fn, to: NoActor.self)
      return rawFn()
    }
  }

  // Otherwise, create a new task on the main actor and run operation there.
  Task { @MainActor in
    operation()
  }
}

#endif
