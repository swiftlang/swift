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

#if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 5.1, *)
@available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
@globalActor public final actor MainActor: GlobalActor {
  public static let shared = MainActor()

  @inlinable
  public nonisolated var unownedExecutor: UnownedSerialExecutor {
    #if compiler(>=5.5) && $BuiltinBuildMainExecutor
    return UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  @inlinable
  public static var sharedUnownedExecutor: UnownedSerialExecutor {
    #if compiler(>=5.5) && $BuiltinBuildMainExecutor
    return UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
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
    #if compiler(>=5.5) && $BuiltinBuildMainExecutor
    return UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
  }

  @inlinable
  public static var sharedUnownedExecutor: UnownedSerialExecutor {
    #if compiler(>=5.5) && $BuiltinBuildMainExecutor
    return UnownedSerialExecutor(Builtin.buildMainActorExecutorRef())
    #else
    fatalError("Swift compiler is incompatible with this SDK version")
    #endif
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
#endif

/// Performs a runtime test to check whether this function was called
/// while on the MainActor. Then the operation is invoked and its
/// result is returned.
///
/// - Attention:
/// This operation is unsafe because if the runtime check fails, the
/// operation may still be invoked off of the MainActor! You can control
/// the behavior of check failure by setting the environment variable
/// `SWIFT_UNEXPECTED_EXECUTOR_LOG_LEVEL` as follows:
///
///   - 0 ignores check failures
///   - 1 will only log a warning (default)
///   - 2 means fatal error
///
/// When in modes other than `0`, a message is output to standard error.
///
@available(SwiftStdlib 5.1, *)
@_unavailableFromAsync(message: "await the call to the @MainActor closure directly")
@_alwaysEmitIntoClient // FIXME: use @backDeploy(before: SwiftStdlib 5.9)
public
func _unsafeAssumeOnMainActor<T>(debugFileName: String = #file,
                                 debugLineNum: Int = #line,
                                 _ operation: @MainActor () throws -> T
                                ) rethrows -> T {
  typealias YesMainActor = @MainActor () throws -> T
  typealias NoMainActor = () throws -> T

  // even if the check fails, it's not guaranteed to be fatal
  _checkExpectedExecutor(debugFileName, debugLineNum, Builtin.buildMainActorExecutorRef())

  // To do the unsafe cast, we have to pretend it's @escaping.
  return try withoutActuallyEscaping(operation) {
    (_ fn: @escaping YesMainActor) throws -> T in
    let rawFn = unsafeBitCast(fn, to: NoMainActor.self)
    return try rawFn()
  }
}
