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

// None of TaskExecutor APIs are available in task-to-thread concurrency model.
#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

/// Configure the current task hierarchy's task executor preference to the passed ``TaskExecutor``,
/// and execute the passed in closure by immediately hopping to that executor.
///
/// ### Task executor preference semantics
/// Task executors influence _where_ nonisolated asynchronous functions, and default actor methods execute.
/// The preferred executor will be used whenever possible, rather than hopping to the global concurrent pool.
///
/// For an in depth discussion of this topic, see ``TaskExecutor``.
///
/// ### Disabling task executor preference
/// Passing `nil` as executor means disabling any preference preference (if it was set) and the task hierarchy
/// will execute without any executor preference until a different preference is set.
///
/// ### Asynchronous function execution semantics in presence of task executor preferences
/// The following diagram illustrates on which executor an `async` function will
/// execute, in presence (or lack thereof) a task executor preference.
///
/// ```
/// [ func / closure ] - /* where should it execute? */
///                               |
///                     +--------------+          +===========================+
///           +-------- | is isolated? | - yes -> | actor has unownedExecutor |
///           |         +--------------+          +===========================+
///           |                                       |                |
///           |                                      yes               no
///           |                                       |                |
///           |                                       v                v
///           |                  +=======================+    /* task executor preference? */
///           |                  | on specified executor |        |                   |
///           |                  +=======================+       yes                  no
///           |                                                   |                   |
///           |                                                   |                   v
///           |                                                   |    +==========================+
///           |                                                   |    | default (actor) executor |
///           |                                                   v    +==========================+
///           v                                     +==============================+
///  /* task executor preference? */ ---- yes ----> | on Task's preferred executor |
///           |                                     +==============================+
///           no
///           |
///           v
///  +===============================+
///  | on global concurrent executor |
///  +===============================+
/// ```
///
/// In short, without a task executor preference, `nonisolated async` functions
/// will execute on the global concurrent executor. If a task executor preference
/// is present, such `nonisolated async` functions will execute on the preferred
/// task executor.
///
/// Isolated functions semantically execute on the actor they are isolated to,
/// however if such actor does not declare a custom executor (it is a "default
/// actor") in presence of a task executor preference, tasks executing on this
/// actor will use the preferred executor as source of threads to run the task,
/// while isolated on the actor.
///
/// ### Example
///
///     Task {
///       // case 0) "no task executor preference"
///
///       // default task executor
///       // ...
///       await SomeDefaultActor().hello() // default executor
///       await ActorWithCustomExecutor().hello() // 'hello' executes on actor's custom executor
///
///       // child tasks execute on default executor:
///       async let x = ...
///       await withTaskGroup(of: Int.self) { group in g.addTask { 7 } }
///
///       await withTaskExecutorPreference(specific) {
///         // case 1) 'specific' task executor preference
///
///         // 'specific' task executor
///         // ...
///         await SomeDefaultActor().hello() // 'hello' executes on 'specific' executor
///         await ActorWithCustomExecutor().hello() // 'hello' executes on actor's custom executor (same as case 0)
///
///         // child tasks execute on 'specific' task executor:
///         async let x = ...
///         await withTaskGroup(of: Int.self) { group in
///           group.addTask { 7 } // child task executes on 'specific' executor
///           group.addTask(executorPreference: globalConcurrentExecutor) { 13 } // child task executes on global concurrent executor
///         }
///
///         // disable the task executor preference:
///         await withTaskExecutorPreference(globalConcurrentExecutor) {
///           // equivalent to case 0) preference is globalConcurrentExecutor
///
///           // default task executor
///           // ...
///           await SomeDefaultActor().hello() // default executor (same as case 0)
///           await ActorWithCustomExecutor().hello() // 'hello' executes on actor's custom executor (same as case 0)
///
///           // child tasks execute on default executor (same as case 0):
///           async let x = ...
///           await withTaskGroup(of: Int.self) { group in group.addTask { 7 } }
///         }
///       }
///     }
///
/// - Parameters:
///   - taskExecutor: the executor to use as preferred task executor for this
///     operation, and any child tasks created inside the `operation` closure.
///     If `nil` it is interpreted as "no preference" and calling this method
///     will have no impact on execution semantics of the `operation`
///   - operation: the operation to execute on the passed executor
/// - Returns: the value returned from the `operation` closure
/// - Throws: if the operation closure throws
/// - SeeAlso: ``TaskExecutor``
@_unavailableInEmbedded
@available(SwiftStdlib 6.0, *)
public func withTaskExecutorPreference<T, Failure>(
  _ taskExecutor: (any TaskExecutor)?,
  isolation: isolated (any Actor)? = #isolation,
  operation: () async throws(Failure) -> T
) async throws(Failure) -> T {
  guard let taskExecutor else {
    // User explicitly passed a "nil" preference, so we invoke the operation
    // as is, which will hop to it's expected executor without any change in
    // executor preference semantics.
    //
    // We allow this in order to easily drive task executor preference from
    // configuration where the preference may be an optional; so users don't
    // have to write two code paths for "if there is a preference and if there
    // isn't".
    return try await operation()
  }

  let taskExecutorBuiltin: Builtin.Executor =
    taskExecutor.asUnownedTaskExecutor().executor

  let record = _pushTaskExecutorPreference(taskExecutorBuiltin)
  defer {
    unsafe _popTaskExecutorPreference(record: record)
  }

  // No need to manually hop to the target executor, because as we execute
  // the operation, its enqueue will respect the attached executor preference.
  return try await operation()
}

// Note: hack to stage out @_unsafeInheritExecutor forms of various functions
// in favor of #isolation. The _unsafeInheritExecutor_ prefix is meaningful
// to the type checker.
//
// This function also doubles as an ABI-compatibility shim predating the
// introduction of #isolation.
@_unavailableInEmbedded
@available(SwiftStdlib 6.0, *)
@_unsafeInheritExecutor // for ABI compatibility
@_silgen_name("$ss26withTaskExecutorPreference_9operationxSch_pSg_xyYaYbKXEtYaKs8SendableRzlF")
public func _unsafeInheritExecutor_withTaskExecutorPreference<T: Sendable>(
  _ taskExecutor: (any TaskExecutor)?,
  operation: @Sendable () async throws -> T
) async rethrows -> T {
  guard let taskExecutor else {
    return try await operation()
  }

  let taskExecutorBuiltin: Builtin.Executor =
    taskExecutor.asUnownedTaskExecutor().executor

  let record = _pushTaskExecutorPreference(taskExecutorBuiltin)
  defer {
    unsafe _popTaskExecutorPreference(record: record)
  }

  return try await operation()
}

@available(SwiftStdlib 6.0, *)
@_unavailableInEmbedded
extension UnsafeCurrentTask {

  /// The current ``TaskExecutor`` preference, if this task has one configured.
  ///
  /// The executor may be used to compare for equality with an expected executor preference.
  ///
  /// The lifetime of an executor is not guaranteed by an ``UnownedTaskExecutor``,
  /// so accessing it must be handled with great case -- and the program must use other
  /// means to guarantee the executor remains alive while it is in use.
  @available(SwiftStdlib 6.0, *)
  public var unownedTaskExecutor: UnownedTaskExecutor? {
    let ref = _getPreferredUnownedTaskExecutor()
    return unsafe UnownedTaskExecutor(ref)
  }
}

// ==== Runtime ---------------------------------------------------------------

@available(SwiftStdlib 6.0, *)
@_silgen_name("swift_task_getPreferredTaskExecutor")
internal func _getPreferredUnownedTaskExecutor() -> Builtin.Executor

typealias TaskExecutorPreferenceStatusRecord = UnsafeRawPointer

@available(SwiftStdlib 6.0, *)
@_silgen_name("swift_task_pushTaskExecutorPreference")
internal func _pushTaskExecutorPreference(_ executor: Builtin.Executor)
  -> TaskExecutorPreferenceStatusRecord

@available(SwiftStdlib 6.0, *)
@_silgen_name("swift_task_popTaskExecutorPreference")
internal func _popTaskExecutorPreference(
  record: TaskExecutorPreferenceStatusRecord
)

/// Get the "undefined" task executor reference.
///
/// It can be used to compare against, and is semantically equivalent to
/// "no preference".
@available(SwiftStdlib 6.0, *)
@usableFromInline
internal func _getUndefinedTaskExecutor() -> Builtin.Executor {
  // Similar to the `_getGenericSerialExecutor` this method relies
  // on the runtime representation of the "undefined" executor
  // to be specifically `{0, 0}` (a null-pointer to an executor and witness
  // table).
  //
  // Rather than call into the runtime to return the
  // `TaskExecutorRef::undefined()`` we this information to bitcast
  // and return it directly.
  unsafe unsafeBitCast((UInt(0), UInt(0)), to: Builtin.Executor.self)
}

#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
