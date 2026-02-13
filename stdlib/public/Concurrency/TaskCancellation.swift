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
@_alwaysEmitIntoClient
nonisolated(nonsending)
public func withTaskCancellationHandler<Return, Failure>(
  operation: nonisolated(nonsending) () async throws(Failure) -> Return,
  onCancel handler: sending () -> Void
) async throws(Failure) -> Return {
  // unconditionally add the cancellation record to the task.
  // if the task was already cancelled, it will be executed right away.
  let record = unsafe Builtin.taskAddCancellationHandler(handler: handler)
  defer { unsafe Builtin.taskRemoveCancellationHandler(record: record) }
  return try await operation()
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
///   - isolation: The actor that the operation and cancellation handler are isolated to.
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
@available(SwiftStdlib 6.0, *)
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
  /// a cancellation shield is active. Use ``Task/isCancelled`` (the static property)
  /// if you need cancellation checking that respects active shields.
  ///
  /// ### Instance property isCancelled ignores Task Cancellation Shields
  ///
  /// Instance properties `task.isCancelled` and `unsafeCurrentTask.isCancelled`
  /// are not contextual and therefore do not respect cancellation shields.
  /// If a task was cancelled and is executing with an active cancellation shield,
  /// these properties will return the _actual_ cancellation status of the task.
  ///
  /// Prefer using `Task.isCancelled` (the static property) in most situations when checking
  /// the cancellation status from inside the task.
  ///
  /// - SeeAlso: ``Task/isCancelled``
  /// - SeeAlso: ``Task/checkCancellation()``
  /// - SeeAlso: ``Task/hasActiveCancellationShield``
  /// - SeeAlso: ``withTaskCancellationShield(operation:)``
  @_transparent
  public var isCancelled: Bool {
    // This is @available(SwiftStdlib 6.4, *) but can't use SwiftStdlib in transparent function
    if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, visionOS 9999, *) {
      let ignoreTaskCancellationShield: UInt64 = 0x1
      return unsafe _taskIsCancelledWithFlags(_task, flags: ignoreTaskCancellationShield)
    } else {
      return _taskIsCancelled(_task)
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
  /// (``withTaskCancellationShield(operation:)``), which may cause `isCancelled`
  /// to return `false` even though the task has been cancelled externally.
  ///
  /// - SeeAlso: ``checkCancellation()``
  /// - SeeAlso: ``withTaskCancellationShield(operation:)``
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

@available(SwiftStdlib 5.1, *)
extension Task where Success == Never, Failure == Never {
  /// Throws an error if the task was canceled.
  ///
  /// The error is always an instance of `CancellationError`.
  ///
  /// - SeeAlso: `isCancelled()`
  @_unavailableInEmbedded
  public static func checkCancellation() throws {
    if Task<Never, Never>.isCancelled {
      throw _Concurrency.CancellationError()
    }
  }
}

/// An error that indicates a task was canceled.
///
/// This error is also thrown automatically by `Task.checkCancellation()`,
/// if the current task has been canceled.
@available(SwiftStdlib 5.1, *)
public struct CancellationError: Error {
  // no extra information, cancellation is intended to be light-weight
  public init() {}
}

@usableFromInline
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_addCancellationHandler")
func _taskAddCancellationHandler(handler: () -> Void) -> UnsafeRawPointer /*CancellationNotificationStatusRecord*/

@usableFromInline
@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_removeCancellationHandler")
func _taskRemoveCancellationHandler(
  record: UnsafeRawPointer /*CancellationNotificationStatusRecord*/
)


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
///     // Child tasks created here do NOT observe the parent's cancellation
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
///     group.addTask { print(Task.isCancelled) } // true - child IS cancelled
///   }
///   group.addTask {
///     withTaskCancellationShield { print(Task.isCancelled) } // false - shielded inside child
///   }
/// }
/// ```
@available(SwiftStdlib 6.4, *)
@_alwaysEmitIntoClient
public nonisolated(nonsending) func withTaskCancellationShield<Value, Failure>(
  operation: nonisolated(nonsending) () async throws(Failure) -> Value,
) async throws(Failure) -> Value {
  let didInstallShield = Builtin.taskCancellationShieldPush()

  defer {
    if Bool(didInstallShield) {
      Builtin.taskCancellationShieldPop()
     }
  }

  return try await operation()
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
///     // Child tasks created here do NOT observe the parent's cancellation
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
///     group.addTask { print(Task.isCancelled) } // true - child IS cancelled
///   }
///   group.addTask {
///     withTaskCancellationShield { print(Task.isCancelled) } // false - shielded inside child
///   }
/// }
/// ```
@available(SwiftStdlib 6.4, *)
@_alwaysEmitIntoClient
public func withTaskCancellationShield<Value, Failure>(
  operation: () throws(Failure) -> Value,
) throws(Failure) -> Value {
  let didInstallShield = Builtin.taskCancellationShieldPush()

  defer {
    if Bool(didInstallShield) {
      Builtin.taskCancellationShieldPop()
    }
  }

  return try operation()
}

@available(SwiftStdlib 6.4, *)
extension Task where Success == Never, Failure == Never {
  /// Checks if the current task is executing in a scope with a task cancellation shield activated by the
  /// ``withTaskCancellationShield(operation:)`` function.
  ///
  /// An active task cancellation shield prevents a task's ability to observe if it was cancelled,
  /// i.e. the ``Task/isCancelled`` property will always return `false` when the task is executing
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
  /// - SeeAlso: ``withTaskCancellationShield(operation:)``
  /// - SeeAlso: ``UnsafeCurrentTask/hasActiveCancellationShield``
  @available(SwiftStdlib 6.4, *)
  @_alwaysEmitIntoClient
  public static var hasActiveCancellationShield: Bool {
    @_alwaysEmitIntoClient
    get {
      unsafe withUnsafeCurrentTask { task in
        unsafe task?.hasActiveCancellationShield ?? false
      }
    }
  }
}
