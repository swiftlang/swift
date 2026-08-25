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

// ==== Task Cancellation ------------------------------------------------------

/// Execute an operation with a cancellation handler that's immediately
/// invoked if the current task is canceled.
///
/// - Parameters:
///   - operation: The operation to perform.
///   - handler: A closure to execute on cancellation.
///     If the task is canceled, this closure is called at most once;
///     otherwise, it isn't called.
///
/// This differs from the operation cooperatively checking for cancellation
/// and reacting to it in that the cancellation handler is _always_ and
/// _immediately_ invoked when the task is canceled. For example, even if the
/// operation is running code that never checks for cancellation, a cancellation
/// handler still runs and provides a chance to run some cleanup code:
///
/// ```
/// await withTaskCancellationHandler {
///   var sum = 0
///   while condition {
///     sum += 1
///   }
///   return sum
/// } onCancel: {
///   // This onCancel closure might execute concurrently with the operation.
///   condition.cancel()
/// }
/// ```
///
/// ### Execution order and semantics
/// The `operation` closure is always invoked, even when the
/// `withTaskCancellationHandler(operation:onCancel:)` method is called from a task
/// that was already canceled.
///
/// When `withTaskCancellationHandler(operation:onCancel:)` is used in a task that has already been
/// canceled, the cancellation handler will be executed
/// immediately before the `operation` closure gets to execute.
///
/// This allows the cancellation handler to set some external "canceled" flag
/// that the operation may be *atomically* checking for in order to avoid
/// performing any actual work once the operation gets to run.
///
/// The `operation` closure executes on the calling execution context, and doesn't
/// suspend or change execution context unless code contained within the closure
/// does so. In other words, the potential suspension point of the
/// `withTaskCancellationHandler(operation:onCancel:)` never suspends by itself before
/// executing the operation.
///
/// If cancellation occurs while the operation is running, the cancellation
/// handler executes *concurrently* with the operation.
///
/// ### Cancellation handlers and locks
///
/// Cancellation handlers which acquire locks must take care to avoid deadlock.
/// The cancellation handler may be invoked while holding internal locks
/// associated with the task or other tasks.  Other operations on the task, such
/// as resuming a continuation, may acquire these same internal locks.
/// Therefore, if a cancellation handler must acquire a lock, other code should
/// not cancel tasks or resume continuations while holding that lock.
@available(SwiftStdlib 5.1, *)
@export(implementation)
public nonisolated(nonsending) func withTaskCancellationHandler<Return, Failure>(
  operation: nonisolated(nonsending) () async throws(Failure) -> Return,
  onCancel handler: sending () -> Void
) async throws(Failure) -> Return {
  // unconditionally add the cancellation record to the task.
  // if the task was already cancelled, it will be executed right away.
#if $BuiltinConcurrencyStackNesting
  let record = unsafe Builtin.taskAddCancellationHandler(handler: handler)
  defer { unsafe Builtin.taskRemoveCancellationHandler(record: record) }
#else
  let record = unsafe _taskAddCancellationHandler(handler: handler)
  defer { unsafe _taskRemoveCancellationHandler(record: record) }
#endif
  return try await operation()
}

/// Execute an operation with a cancellation handler that's immediately
/// invoked with the cancellation reason when the current task is canceled.
///
/// - Parameters:
///   - operation: The operation to perform.
///   - handler: A closure to execute on cancellation, passed the reason the
///     task was cancelled.
///     If the task is canceled, this closure is called at most once;
///     otherwise, it isn't called.
///
/// This differs from the operation cooperatively checking for cancellation
/// and reacting to it in that the cancellation handler is _always_ and
/// _immediately_ invoked when the task is canceled. For example, even if the
/// operation is running code that never checks for cancellation, a cancellation
/// handler still runs and provides a chance to run some cleanup code:
///
/// ```
/// await withTaskCancellationHandler {
///   var sum = 0
///   while condition {
///     sum += 1
///   }
///   return sum
/// } onCancel: { reason in
///   // This onCancel closure might execute concurrently with the operation.
///   condition.cancel()
/// }
/// ```
///
/// ### Execution order and semantics
/// The `operation` closure is always invoked, even when the
/// `withTaskCancellationHandler(operation:onCancel:)` method is called from a task
/// that was already canceled.
///
/// When `withTaskCancellationHandler(operation:onCancel:)` is used in a task that has already been
/// canceled, the cancellation handler will be executed
/// immediately before the `operation` closure gets to execute.
///
/// This allows the cancellation handler to set some external "canceled" flag
/// that the operation may be *atomically* checking for in order to avoid
/// performing any actual work once the operation gets to run.
///
/// The `operation` closure executes on the calling execution context, and doesn't
/// suspend or change execution context unless code contained within the closure
/// does so. In other words, the potential suspension point of the
/// `withTaskCancellationHandler(operation:onCancel:)` never suspends by itself before
/// executing the operation.
///
/// If cancellation occurs while the operation is running, the cancellation
/// handler executes *concurrently* with the operation.
///
/// The reason passed to `handler` is the same value that `Task.cancellationReason`
/// would report on the task being cancelled. However, the handler is invoked
/// from the *cancelling* context, not from the cancelled task's own context,
/// so checking `Task.cancellationReason` from inside the `onCancel` closure
/// would report the cancellation status of the cancelling context rather than
/// that of the task being cancelled; use the passed-in `reason` parameter,
/// which does represent the cancelled task's status.
///
/// ### Cancellation handlers and locks
///
/// Cancellation handlers which acquire locks must take care to avoid deadlock.
/// The cancellation handler may be invoked while holding internal locks
/// associated with the task or other tasks.  Other operations on the task, such
/// as resuming a continuation, may acquire these same internal locks.
/// Therefore, if a cancellation handler must acquire a lock, other code should
/// not cancel tasks or resume continuations while holding that lock.
@available(StdlibDeploymentTarget 6.5, *)
@export(implementation)
public nonisolated(nonsending) func withTaskCancellationHandler<Return, Failure>(
  operation: nonisolated(nonsending) () async throws(Failure) -> Return,
  onCancel handler: sending (CancellationError.Reason) -> Void
) async throws(Failure) -> Return
  where Return: ~Copyable,
        Failure: Error {
  return try await __withTaskCancellationHandlerWithReason0(
    operation: operation,
    onCancel: {
      handler(CancellationError.Reason(_rawValue: $0) ?? .unspecified)
    })
}

// Method necessary in order to avoid the handler0 to be destroyed too eagerly.
@available(StdlibDeploymentTarget 6.5, *)
@export(implementation)
nonisolated(nonsending) func __withTaskCancellationHandlerWithReason0<Return, Failure>(
  operation: nonisolated(nonsending) () async throws(Failure) -> Return,
  onCancel handler0: sending (UInt8) -> Void
) async throws(Failure) -> Return
  where Return: ~Copyable,
        Failure: Error {
  // unconditionally add the cancellation record to the task.
  // if the task was already cancelled, it will be executed right away.
#if $BuiltinCancellationHandlerWithReason
  let record = unsafe Builtin.taskAddCancellationHandlerWithReason(handler: handler0)
  defer { unsafe Builtin.taskRemoveCancellationHandler(record: record) }
  return try await operation()
#else
  fatalError("Swift compiler is incompatible with this SDK version")
#endif
}

#if !$Embedded
/// Execute an operation with a cancellation handler that's immediately
/// invoked if the current task is canceled.
///
/// - Parameters:
///   - operation: The operation to perform.
///   - handler: A closure to execute on cancellation.
///     If the task is canceled, this closure is called at most once;
///     otherwise, it isn't called.
///   - isolation: The actor that the operation is isolated to.
///
/// This differs from the operation cooperatively checking for cancellation
/// and reacting to it in that the cancellation handler is _always_ and
/// _immediately_ invoked when the task is canceled. For example, even if the
/// operation is running code that never checks for cancellation, a cancellation
/// handler still runs and provides a chance to run some cleanup code:
///
/// ```
/// await withTaskCancellationHandler {
///   var sum = 0
///   while condition {
///     sum += 1
///   }
///   return sum
/// } onCancel: {
///   // This onCancel closure might execute concurrently with the operation.
///   condition.cancel()
/// }
/// ```
///
/// ### Execution order and semantics
/// The `operation` closure is always invoked, even when the
/// `withTaskCancellationHandler(operation:onCancel:)` method is called from a task
/// that was already canceled.
///
/// When `withTaskCancellationHandler(operation:onCancel:)` is used in a task that has already been
/// canceled, the cancellation handler will be executed
/// immediately before the `operation` closure gets to execute.
///
/// This allows the cancellation handler to set some external "canceled" flag
/// that the operation may be *atomically* checking for in order to avoid
/// performing any actual work once the operation gets to run.
///
/// The `operation` closure executes on the calling execution context, and doesn't
/// suspend or change execution context unless code contained within the closure
/// does so. In other words, the potential suspension point of the
/// `withTaskCancellationHandler(operation:onCancel:)` never suspends by itself before
/// executing the operation.
///
/// If cancellation occurs while the operation is running, the cancellation
/// handler executes *concurrently* with the operation.
///
/// ### Cancellation handlers and locks
///
/// Cancellation handlers which acquire locks must take care to avoid deadlock.
/// The cancellation handler may be invoked while holding internal locks
/// associated with the task or other tasks.  Other operations on the task, such
/// as resuming a continuation, may acquire these same internal locks.
/// Therefore, if a cancellation handler must acquire a lock, other code should
/// not cancel tasks or resume continuations while holding that lock.
@available(SwiftStdlib 5.1, *)
@backDeployed(before: SwiftStdlib 6.0)
public func withTaskCancellationHandler<T>(
  operation: () async throws -> T,
  onCancel handler: @Sendable () -> Void,
  isolation: isolated (any Actor)?
) async rethrows -> T {
  // unconditionally add the cancellation record to the task.
  // if the task was already cancelled, it will be executed right away.
#if $BuiltinConcurrencyStackNesting
  let record = unsafe Builtin.taskAddCancellationHandler(handler: handler)
  defer { unsafe Builtin.taskRemoveCancellationHandler(record: record) }
#else
  let record = unsafe _taskAddCancellationHandler(handler: handler)
  defer { unsafe _taskRemoveCancellationHandler(record: record) }
#endif
  return try await operation()
}
#endif

// Note: hack to stage out @_unsafeInheritExecutor forms of various functions
// in favor of #isolation. The _unsafeInheritExecutor_ prefix is meaningful
// to the type checker.
//
// This function also doubles as an ABI-compatibility shim predating the
// introduction of #isolation.
@_unsafeInheritExecutor // ABI compatibility with Swift 5.1
@available(SwiftStdlib 5.1, *)
@_silgen_name("$ss27withTaskCancellationHandler9operation8onCancelxxyYaKXE_yyYbXEtYaKlF")
public func _unsafeInheritExecutor_withTaskCancellationHandler<T>(
  operation: () async throws -> T,
  onCancel handler: @Sendable () -> Void
) async rethrows -> T {
  // unconditionally add the cancellation record to the task.
  // if the task was already cancelled, it will be executed right away.
#if $BuiltinConcurrencyStackNesting
  let record = unsafe Builtin.taskAddCancellationHandler(handler: handler)
  defer { unsafe Builtin.taskRemoveCancellationHandler(record: record) }
#else
  let record = unsafe _taskAddCancellationHandler(handler: handler)
  defer { unsafe _taskRemoveCancellationHandler(record: record) }
#endif
  return try await operation()
}

@available(SwiftStdlib 5.1, *)
extension Task {
  /// A Boolean value that indicates whether the task should stop executing.
  ///
  /// After the value of this property becomes `true`, it remains `true` indefinitely.
  /// There is no way to uncancel a task.
  ///
  /// This property returns the actual cancellation state of the task, regardless of whether
  /// a cancellation shield is active. Use ``Task/isCancelled-type.property`` (the static property)
  /// if you need cancellation checking that respects active shields.
  ///
  /// ### Instance property isCancelled ignores Task Cancellation Shields
  ///
  /// The instance property ``Task/isCancelled-property``
  /// is not contextual and therefore does not respect cancellation shields.
  /// If a task was cancelled and is executing with an active cancellation shield,
  /// these properties will return the _actual_ cancellation status of the specific task.
  ///
  /// Prefer using ``Task/isCancelled-type.property`` (the static property) in most situations when checking
  /// the cancellation status from inside the task.
  ///
  /// - SeeAlso: ```Task/isCancelled-type.property``
  /// - SeeAlso: ``Task/checkCancellation()``
  /// - SeeAlso: ``Task/hasActiveCancellationShield``
  /// - SeeAlso: ``withTaskCancellationShield(operation:)-(()->Value)``
  @_transparent
  public var isCancelled: Bool {
    // This is @available(SwiftStdlib 6.4, *) but can't use SwiftStdlib in transparent function
    if #available(anyAppleOS 27, *) {
      let ignoreTaskCancellationShield: UInt64 = 0x1
      return unsafe _taskIsCancelledWithFlags(_AsyncTask(_task), flags: ignoreTaskCancellationShield)
    } else {
      return unsafe _taskIsCancelled(_AsyncTask(_task))
    }
  }
}

@available(SwiftStdlib 5.1, *)
extension Task where Success == Never, Failure == Never {
  /// A Boolean value that indicates whether the task should stop executing.
  ///
  /// After the value of this property becomes `true`, it remains `true` indefinitely.
  /// There is no way to uncancel a task.
  ///
  /// ### Interaction with Task Cancellation Shields
  ///
  /// Cancellation may be suppressed by an active task cancellation shield
  /// (``withTaskCancellationShield(operation:)-(()->Value)``), which may cause `isCancelled`
  /// to return `false` even though the task has been cancelled externally.
  ///
  /// - SeeAlso: ``checkCancellation()``
  /// - SeeAlso: ``withTaskCancellationShield(operation:)-(()->Value)``
  public static var isCancelled: Bool {
    unsafe withUnsafeCurrentTask { task in
      if #available(SwiftStdlib 6.4, *) {
        unsafe task?._isCancelled(ignoreTaskCancellationShield: false) ?? false
      } else {
        unsafe task?.isCancelled ?? false
      }
    }
  }
}

@available(StdlibDeploymentTarget 6.5, *)
extension Task where Success == Never, Failure == Never {
  /// The reason for the current task's cancellation, or `nil` if the task is
  /// not cancelled.
  ///
  /// Mirrors ``Task/isCancelled``: once this returns a non-nil value it will
  /// consistently return the same value for the remaining life of the task.
  ///
  /// Reading this from outside the context of a task returns `nil`.
  ///
  /// - Returns: The ``CancellationError/Reason`` that was passed to the
  ///   originating cancellation call (e.g. via
  ///   ``Task/cancel(reason:)``, ``TaskGroup/cancelAll(reason:)``, or
  ///   ``TaskCancellationScope/cancel(reason:)``), or
  ///   ``CancellationError/Reason/unspecified`` for tasks cancelled
  ///   through a reasonless entry point. `nil` if the current task is
  ///   not cancelled, or if there is no current task.
  ///
  /// - SeeAlso: ``Task/isCancelled``
  /// - SeeAlso: ``CancellationError/Reason``
  @available(StdlibDeploymentTarget 6.5, *)
  @export(implementation)
  public static var cancellationReason: CancellationError.Reason? {
    unsafe withUnsafeCurrentTask { task in
      unsafe task?.cancellationReason
    }
  }
}

@available(SwiftStdlib 5.1, *)
extension Task where Success == Never, Failure == Never {
  /// Throws an error if the task was canceled.
  ///
  /// The error is always an instance of `CancellationError`. Its `reason`
  /// reports why the task was cancelled: for example, `.deadlineExpired`
  /// when the current call site is inside a `withDeadline` block whose
  /// deadline has elapsed; `.unspecified` otherwise.
  ///
  /// - SeeAlso: `isCancelled()`
  /// - SeeAlso: ``CancellationError/Reason``
  @_unavailableInEmbedded
  public static func checkCancellation() throws {
    if Task<Never, Never>.isCancelled {
      if #available(StdlibDeploymentTarget 6.5, *) {
        throw _Concurrency.CancellationError(
          reason: Task.cancellationReason ?? .unspecified)
      } else {
        throw _Concurrency.CancellationError()
      }
    }
  }
}

/// An error that indicates a task was canceled.
///
/// This error is also thrown automatically by `Task.checkCancellation()`,
/// if the current task has been canceled.
@available(SwiftStdlib 5.1, *)
public struct CancellationError: Error {
  /// Raw storage containing the reason's `CancellationError.Reason.rawValue`;
  /// We cannot store the enum directly because of its availability.
  @usableFromInline
  internal var _reasonRawStorage: UInt8 = 0x00

  // no extra information, cancellation is intended to be light-weight
  public init() {}
}

@available(StdlibDeploymentTarget 6.5, *)
extension CancellationError: CustomStringConvertible {
  @available(StdlibDeploymentTarget 6.5, *)
  public var description: String {
    "CancellationError(reason: \(reason))"
  }
}

@available(StdlibDeploymentTarget 6.5, *)
extension CancellationError {
  /// Describes why a task was cancelled.
  ///
  /// This enum is non-frozen, and additional cases may be added in future versions.
  ///
  /// - SeeAlso: `Task.cancellationReason`
  /// - SeeAlso: `Task.cancel(reason:)`
  @available(StdlibDeploymentTarget 6.5, *)
  @nonexhaustive
  public enum Reason: Sendable, Hashable, CaseIterable,
                      CustomStringConvertible, CustomDebugStringConvertible {
    // Not explicitly `: UInt8` because we want to leave it extensible just in case.

    /// The task was cancelled without a specific reason being provided.
    ///
    /// This is the reason produced by the plain `Task.cancel()` /
    /// `UnsafeCurrentTask.cancel()` / `TaskGroup.cancelAll()` entry points,
    /// as well as anything upstream that propagates cancellation without
    /// supplying a reason.
    case unspecified

    /// The task was cancelled because a `withDeadline` block's deadline
    /// elapsed.
    case deadlineExpired

    @available(StdlibDeploymentTarget 6.5, *)
    public var description: String {
      switch self {
      case .unspecified: return "unspecified"
      case .deadlineExpired: return "deadlineExpired"
      }
    }

    @available(StdlibDeploymentTarget 6.5, *)
    public var debugDescription: String {
      "CancellationError.Reason.\(description)"
    }
  }

  /// Create a `CancellationError` with a specific `Reason`.
  ///
  /// The `reason` is then accessible via the error's `reason` property.
  @available(StdlibDeploymentTarget 6.5, *)
  @export(implementation)
  public init(reason: Reason) {
    self.init()
    self._reasonRawStorage = reason._rawValue
  }

  /// The reason this task was cancelled.
  ///
  /// Errors constructed via the zero-argument `init()` (either directly or
  /// by the runtime, e.g. when `Task.checkCancellation()` throws) report
  /// `.unspecified`. Errors constructed via `init(reason:)` report the
  /// specified reason.
  @available(StdlibDeploymentTarget 6.5, *)
  @export(implementation)
  public var reason: Reason {
    Reason(_rawValue: _reasonRawStorage) ?? .unspecified
  }
}

@available(StdlibDeploymentTarget 6.5, *)
extension CancellationError.Reason {
  /// The stable numeric encoding used on the wire between Swift and the C++
  /// runtime (low bits of `swift_task_cancelWithFlags`' flags, scope-record
  /// state's packed reason field, etc.). Hard-coded so future non-frozen
  /// cases can be added without perturbing the ABI.
  @export(implementation)
  internal var _rawValue: UInt8 {
    switch self {
    case .unspecified: return 0
    case .deadlineExpired: return 1
    @unknown default:
      fatalError("Unknown CancellationError.Reason: \(self)")
    }
  }

  /// Decode a raw value coming from the runtime. Returns nil for unknown
  /// values so callers can decide the fallback policy (e.g. `.unspecified`).
  @export(implementation)
  internal init?(_rawValue: UInt8) {
    switch _rawValue {
    case 0: self = .unspecified
    case 1: self = .deadlineExpired
    default: return nil
    }
  }
}

@usableFromInline
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_addCancellationHandler")
func _taskAddCancellationHandler(handler: () -> Void) -> UnsafeRawPointer /*CancellationNotificationStatusRecord*/

@usableFromInline
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_removeCancellationHandler")
func _taskRemoveCancellationHandler(record: UnsafeRawPointer /*any cancellation notification record*/)


// ==== Task Cancellation Shielding -------------------------------------------

/// Enters a scope in which a task cancellation shield is active.
///
/// Cancellation shields are primarily used to ensure some cleanup code will
/// definitely run, even if the context in which the cleanup functions are called from
/// is a cancelled task, and the functions may otherwise return early (due to observing
/// the cancellation of the current task).
///
/// For example, a resource cleanup function might internally check for cancellation,
/// which could cause it to skip important cleanup work:
///
/// ```swift
/// let resource = await makeResource()
/// defer {
///   await withTaskCancellationShield {
///     await resource.finish() // runs to completion, even if task was cancelled earlier
///   }
/// }
///
/// struct Resource {
///   func finish() {
///     guard !Task.isCancelled() else { return } // returns early if task was cancelled!
///     // real work happens here
///   }
/// ```
///
/// While inside a cancellation shield, `Task.isCancelled` returns `false` and
/// `Task.checkCancellation()` does not throw, even if the surrounding task
/// has been cancelled. Similarly task cancellation handlers do not trigger
/// while executing in a shielded block of code.
///
/// Once the shield scope exits, the task's actual
/// cancellation status becomes observable again. Cancellation shields to not
/// prevent the task from becoming cancelled, but only prevent observing the
/// cancellation while executing inside a shielded scope.
///
/// Cancellation shields also prevent cancellation from propagating to child tasks
/// created within the shielded scope:
///
/// ```swift
/// let task = Task {
///   withUnsafeCurrentTask { $0?.cancel() } // cancel the task
///
///   await withTaskCancellationShield {
///     // Child tasks created here do _not_ observe the parent's cancellation
///     // and therefore start as not cancelled. They can be individually cancelled though.
///     await withTaskGroup(of: Void.self) { group in
///       group.addTask {
///         print(Task.isCancelled) // false
///       }
///       for await _ in group {}
///
///       group.cancelAll() // explicitly cancelling the group does cancel child tasks of the group
///       group.addTask {
///         print(Task.isCancelled) // true
///       }
///     }
///   }
/// }
/// ```
///
/// Note that shielding the `addTask` call itself does not shield the child task:
///
/// ```swift
/// await withTaskGroup(of: Void.self) { group in
///   group.cancelAll()
///   withTaskCancellationShield {
///     group.addTask { print(Task.isCancelled) } // true - child is cancelled
///   }
///   group.addTask {
///     withTaskCancellationShield { print(Task.isCancelled) } // false - shielded inside child
///   }
/// }
/// ```
@available(SwiftStdlib 6.4, *)
@export(implementation)
public nonisolated(nonsending) func withTaskCancellationShield<Value, Failure>(
  operation: nonisolated(nonsending) () async throws(Failure) -> Value,
) async throws(Failure) -> Value {
#if $BuiltinTaskCancellationShield
  let didInstallShield = Builtin.taskCancellationShieldPush()

  defer {
    if Bool(didInstallShield) {
      Builtin.taskCancellationShieldPop()
     }
  }

  return try await operation()
#else
  fatalError("Swift compiler is incompatible with this SDK version")
#endif
}


/// Enters a scope in which a task cancellation shield is active.
///
/// Cancellation shields are primarily used to ensure some cleanup code will
/// definitely run, even if the context in which the cleanup functions are called from
/// is a cancelled task, and the functions may otherwise return early (due to observing
/// the cancellation of the current task).
///
/// For example, a resource cleanup function might internally check for cancellation,
/// which could cause it to skip important cleanup work:
///
/// ```swift
/// let resource = await makeResource()
/// defer {
///   await withTaskCancellationShield {
///     await resource.finish() // runs to completion, even if task was cancelled earlier
///   }
/// }
///
/// struct Resource {
///   func finish() {
///     guard !Task.isCancelled() else { return } // returns early if task was cancelled!
///     // real work happens here
///   }
/// ```
///
/// While inside a cancellation shield, `Task.isCancelled` returns `false` and
/// `Task.checkCancellation()` does not throw, even if the surrounding task
/// has been cancelled. Similarly task cancellation handlers do not trigger
/// while executing in a shielded block of code.
///
/// Once the shield scope exits, the task's actual
/// cancellation status becomes observable again. Cancellation shields to not
/// prevent the task from becoming cancelled, but only prevent observing the
/// cancellation while executing inside a shielded scope.
///
/// Cancellation shields also prevent cancellation from propagating to child tasks
/// created within the shielded scope:
///
/// ```swift
/// let task = Task {
///   withUnsafeCurrentTask { $0?.cancel() } // cancel the task
///
///   await withTaskCancellationShield {
///     // Child tasks created here do _not_ observe the parent's cancellation
///     // and therefore start as not cancelled. They can be individually cancelled though.
///     await withTaskGroup(of: Void.self) { group in
///       group.addTask {
///         print(Task.isCancelled) // false
///       }
///       for await _ in group {}
///
///       group.cancelAll() // explicitly cancelling the group does cancel child tasks of the group
///       group.addTask {
///         print(Task.isCancelled) // true
///       }
///     }
///   }
/// }
/// ```
///
/// Note that shielding the `addTask` call itself does not shield the child task:
///
/// ```swift
/// await withTaskGroup(of: Void.self) { group in
///   group.cancelAll()
///   withTaskCancellationShield {
///     group.addTask { print(Task.isCancelled) } // true - child is cancelled
///   }
///   group.addTask {
///     withTaskCancellationShield { print(Task.isCancelled) } // false - shielded inside child
///   }
/// }
/// ```
@available(SwiftStdlib 6.4, *)
@export(implementation)
public func withTaskCancellationShield<Value, Failure>(
  operation: () throws(Failure) -> Value,
) throws(Failure) -> Value {
#if $BuiltinTaskCancellationShield
  let didInstallShield = Builtin.taskCancellationShieldPush()

  defer {
    if Bool(didInstallShield) {
      Builtin.taskCancellationShieldPop()
    }
  }

  return try operation()
#else
  fatalError("Swift compiler is incompatible with this SDK version")
#endif
}

@available(SwiftStdlib 6.4, *)
extension Task where Success == Never, Failure == Never {
  /// Checks if the current task is executing in a scope with a task cancellation shield activated by the
  /// ``withTaskCancellationShield(operation:)-(()->Value)`` function.
  ///
  /// An active task cancellation shield prevents a task's ability to observe if it was cancelled,
  /// i.e. the ``Task/isCancelled-type.property`` property will always return `false` when the task is executing
  /// with an active shield.
  ///
  /// This property is primarily aimed at  debugging and understanding cancellation behavior
  /// in complex call hierarchies, and should not be used in regular control flow.
  ///
  /// Returns `true` when executing within a task that has an active cancellation shield.
  ///
  /// Cancellation shields are not automatically inherited by child tasks; each child task must install
  /// its own shield if needed if it, independently, wanted to ignore cancellation during a specific scope.
  ///
  /// - SeeAlso: ``withTaskCancellationShield(operation:)-(()->Value)``
  /// - SeeAlso: ``UnsafeCurrentTask/hasActiveCancellationShield``
  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  public static var hasActiveCancellationShield: Bool {
    @export(implementation)
    get {
      unsafe withUnsafeCurrentTask { task in
        unsafe task?.hasActiveCancellationShield ?? false
      }
    }
  }
}
