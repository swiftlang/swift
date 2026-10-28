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

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 5.1, *)
@_unavailableInEmbedded
extension Task where Success == Never, Failure == Never {
  @available(*, deprecated, renamed: "Task.sleep(nanoseconds:)")
  /// Suspends the current task for at least the given duration
  /// in nanoseconds.
  ///
  /// This function doesn't block the underlying thread.
  public static func sleep(_ duration: UInt64) async {
    return await Builtin.withUnsafeContinuation {
      (continuation: Builtin.RawUnsafeContinuation) -> Void in
      let job = _taskCreateNullaryContinuationJob(
          priority: Int(Task.currentPriority.rawValue),
          continuation: continuation)

      if #available(StdlibDeploymentTarget 6.5, *) {
        #if !$Embedded
        if let executor = Task.currentSchedulingExecutor,
           executor.supportsScheduling(on: .continuous) {
          // This sleep can't be cancelled, so we have no use for the token.
          _ = executor.schedule(ExecutorJob(context: job),
                                at: .after(.nanoseconds(duration)),
                                clock: .continuous)
          return
        }
        #endif
      }

      // If there is no current scheduling executor, fall back to
      // _enqueueJobGlobalWithDelay()
      _enqueueJobGlobalWithDelay(duration, job)
    }
  }

  /// Suspends the current task for at least the given duration
  /// in nanoseconds.
  ///
  /// If the task is canceled before the time ends,
  /// this function throws `CancellationError`.
  ///
  /// This function doesn't block the underlying thread.
  @diagnose(UselessAvailabilityCheck, as: ignored)
  public static func sleep(nanoseconds duration: UInt64) async throws {
    #if !$Embedded
    if #available(StdlibDeploymentTarget 6.5, *) {
      try await _sleep(run: .after(.nanoseconds(duration)),
                       tolerance: nil,
                       clock: ContinuousClock())
      return
    }
    #endif
    let clock = ContinuousClock()
    try await _sleepWithoutSchedulingExecutor(
      until: clock.now.advanced(by: .nanoseconds(duration)),
      tolerance: nil,
      clock: clock)
  }
}
#else
@available(SwiftStdlib 5.1, *)
@available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
extension Task where Success == Never, Failure == Never {
  @available(SwiftStdlib 5.1, *)
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func sleep(_ duration: UInt64) async {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
  @available(SwiftStdlib 5.1, *)
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public static func sleep(nanoseconds duration: UInt64) async throws {
    fatalError("Unavailable in task-to-thread concurrency model")
  }
}
#endif
