//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if os(WASI) || !$Embedded

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 5.1, *)
@available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
@globalActor public final actor MainActor: GlobalActor {
  public static let shared = MainActor()

  @inlinable
  public nonisolated var unownedExecutor: UnownedSerialExecutor {
    return unsafe UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
  }

  @inlinable
  public static var sharedUnownedExecutor: UnownedSerialExecutor {
    return unsafe UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
  }

  @inlinable
  public nonisolated func enqueue(_ job: UnownedJob) {
    _enqueueOnMain(job)
  }
}
#else
/// A singleton actor whose executor is equivalent to the main
/// dispatch queue.
@available(SwiftStdlib 5.1, *)
@globalActor public final actor MainActor: GlobalActor {
  public static let shared = MainActor()

  @inlinable
  public nonisolated var unownedExecutor: UnownedSerialExecutor {
    return unsafe UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
  }

  @inlinable
  public static var sharedUnownedExecutor: UnownedSerialExecutor {
    return unsafe UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
  }

  @inlinable
  public nonisolated func enqueue(_ job: UnownedJob) {
    _enqueueOnMain(job)
  }
}
#endif

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 5.1, *)
extension MainActor {
  /// Execute the given body closure on the main actor.
  ///
  /// Historical ABI entry point, superseded by the Sendable version that is
  /// also inlined to back-deploy a semantic fix where this operation would
  /// not hop back at the end.
  @usableFromInline
  static func run<T>(
    resultType: T.Type = T.self,
    body: @MainActor @Sendable () throws -> T
  ) async rethrows -> T {
    return try await body()
  }

  /// Execute the given body closure on the main actor.
  @_alwaysEmitIntoClient
  public static func run<T: Sendable>(
    resultType: T.Type = T.self,
    body: @MainActor @Sendable () throws -> T
  ) async rethrows -> T {
    return try await body()
  }
}

@available(SwiftStdlib 5.1, *)
extension MainActor {
  /// Assume that the current task is executing on the main actor's
  /// serial executor, or stop program execution.
  ///
  /// This method allows to *assume and verify* that the currently
  /// executing synchronous function is actually executing on the serial
  /// executor of the MainActor.
  ///
  /// If that is the case, the operation is invoked with an `isolated` version
  /// of the actor, / allowing synchronous access to actor local state without
  /// hopping through asynchronous boundaries.
  ///
  /// If the current context is not running on the actor's serial executor, or
  /// if the actor is a reference to a remote actor, this method will crash
  /// with a fatal error (similar to ``preconditionIsolated()``).
  ///
  /// This method can only be used from synchronous functions, as asynchronous
  /// functions should instead perform a normal method call to the actor, which
  /// will hop task execution to the target actor if necessary.
  ///
  /// - Note: This check is performed against the MainActor's serial executor,
  ///   meaning that / if another actor uses the same serial executor--by using
  ///   ``MainActor/sharedUnownedExecutor`` as its own
  ///   ``Actor/unownedExecutor``--this check will succeed , as from a concurrency
  ///   safety perspective, the serial executor guarantees mutual exclusion of
  ///   those two actors.
  ///
  /// - Parameters:
  ///   - operation: the operation that will be executed if the current context
  ///                is executing on the MainActor's serial executor.
  ///   - file: The file name to print if the assertion fails. The default is
  ///           where this method was called.
  ///   - line: The line number to print if the assertion fails The default is
  ///           where this method was called.
  /// - Returns: the return value of the `operation`
  /// - Throws: rethrows the `Error` thrown by the operation if it threw
  @available(SwiftStdlib 5.1, *)
  @_alwaysEmitIntoClient
  @_unavailableFromAsync(message: "await the call to the @MainActor closure directly")
  public static func assumeIsolated<T : Sendable>(
      _ operation: @MainActor () throws -> T,
      file: StaticString = #fileID, line: UInt = #line
  ) rethrows -> T {
    typealias YesActor = @MainActor () throws -> T
    typealias NoActor = () throws -> T

    /// This is guaranteed to be fatal if the check fails,
    /// as this is our "safe" version of this API.
    let executor: Builtin.Executor = unsafe Self.shared.unownedExecutor.executor
    guard _taskIsCurrentExecutor(executor) else {
      // TODO: offer information which executor we actually got
      #if !$Embedded
      fatalError("Incorrect actor executor assumption; Expected same executor as \(self).", file: file, line: line)
      #else
      Builtin.int_trap()
      #endif
    }

    // To do the unsafe cast, we have to pretend it's @escaping.
    return try withoutActuallyEscaping(operation) {
      (_ fn: @escaping YesActor) throws -> T in
      let rawFn = unsafe unsafeBitCast(fn, to: NoActor.self)
      return try rawFn()
    }
  }

  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  @_silgen_name("$sScM14assumeIsolated_4file4linexxyKScMYcXE_s12StaticStringVSutKlFZ")
  internal static func __abi__assumeIsolated<T : Sendable>(
      _ operation: @MainActor () throws -> T,
      _ file: StaticString, _ line: UInt
  ) rethrows -> T {
    try assumeIsolated(operation, file: file, line: line)
  }
}

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS) || os(visionOS)
@_extern(c, "pthread_main_np")
@usableFromInline
internal func pthread_main_np() -> CInt

@available(SwiftStdlib 5.1, *)
@_alwaysEmitIntoClient
@_silgen_name("swift_task_deinitOnExecutorMainActorBackDeploy")
public func _deinitOnExecutorMainActorBackDeploy(
  _ object: __owned AnyObject,
  _ work: @convention(thin) (__owned AnyObject) -> Void,
  _ executor: Builtin.Executor,
  _ flags: Builtin.Word) {
  if #available(macOS 15.4, iOS 18.4, watchOS 11.4, tvOS 18.4, visionOS 2.4, *) {
    // On new-enough platforms, use the runtime functionality, which allocates
    // the task more efficiently.
    _deinitOnExecutor(object, work, executor, flags)
  } else if pthread_main_np() == 1 {
    // Using "main thread" as a proxy for "main actor", immediately destroy
    // the object.
    work(consume object)
  } else {
    // Steal the local object so that the reference count stays at 1 even when
    // the object is captured.
    var stolenObject: AnyObject? = consume object
    Task.detached { @MainActor in
      work(stolenObject.take()!)
    }
  }
}
#endif

#endif // !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

#endif // os(WASI) || !$Embedded
