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
/// Refer to ``TaskExecutor`` documentation for an in-depth explanation about executor selection mechanisms.
///
/// - Parameters:
///   - taskExecutor: The preferred task executor for this operation, and any
///     child tasks created inside the `operation` closure.
///     Pass ``globalConcurrentExecutor`` to explicitly prefer the global
///     concurrent executor (e.g. to override an outer task's preference).
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
    unsafe taskExecutor.asUnownedTaskExecutor().executor

  let record = unsafe _pushTaskExecutorPreference(taskExecutorBuiltin)
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
    unsafe taskExecutor.asUnownedTaskExecutor().executor

  let record = unsafe _pushTaskExecutorPreference(taskExecutorBuiltin)
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
