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
    #if !$Embedded
      if #available(StdlibDeploymentTarget 9999, *) {
        let clock = ContinuousClock()
        try? await clock.sleep(
          until: clock.now.advanced(by: .nanoseconds(duration)),
          tolerance: nil
        )
      } else {
        fatalError("Unable to sleep because of availability issue")
      }
    #else
      fatalError("Cannot sleep on Embedded Swift")
    #endif
  }

  /// Suspends the current task for at least the given duration
  /// in nanoseconds.
  ///
  /// If the task is canceled before the time ends,
  /// this function throws `CancellationError`.
  ///
  /// This function doesn't block the underlying thread.
  public static func sleep(nanoseconds duration: UInt64) async throws {
    #if !$Embedded
      if #available(StdlibDeploymentTarget 9999, *) {
        let clock = ContinuousClock()
        try await clock.sleep(
          until: clock.now.advanced(by: .nanoseconds(duration)),
          tolerance: nil
        )
      } else {
        fatalError("Unable to sleep because of availability issue")
      }
    #else
      fatalError("Cannot sleep on Embedded Swift")
    #endif
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
