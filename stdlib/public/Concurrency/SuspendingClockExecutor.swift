//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

/// An executor that can schedule jobs and continuations against the suspending clock.
@_spi(ExperimentalScheduling)
@available(StdlibDeploymentTarget 9999, *)
public protocol SuspendingClockExecutor: OperationExecutor {
  /// Enqueues a continuation to be resumed at an instant on the suspending clock.
  ///
  /// The executor takes ownership of the ``ExecutorContinuation`` and must
  /// resume it exactly once: with `.success` at (or after) `instant`, or by
  /// throwing `CancellationError` if ``cancel(_:)`` is called
  /// first.
  ///
  /// - Parameters:
  ///   - continuation: The continuation to resume; the executor takes ownership
  ///     and must resume it exactly once.
  ///   - instant: The continuous-clock instant at (or after) which to resume.
  ///   - tolerance: The permitted timing slack, or `nil` for none.
  /// - Returns: A registration identifying the operation, usable with
  ///   ``OperationExecutor/cancel(_:)`` and
  ///   ``OperationExecutor/escalatePriority(of:to:)``.
  @available(SwiftStdlib 9999, *)
  func enqueue(
    _ continuation: consuming ExecutorContinuation<Void, CancellationError>,
    at instant: SuspendingClock.Instant,
    tolerance: SuspendingClock.Duration?
  ) -> OperationExecutorRegistration

  /// Enqueues a job to be resumed at an instant on the suspending clock.
  ///
  /// The executor takes ownership of the ``ExecutorJob`` and runs it once at (or after) `instant`.
  ///
  /// - Parameters:
  ///   - job: The job to run; the executor takes ownership and runs it once.
  ///   - instant: The continuous-clock instant at (or after) which to resume.
  ///   - tolerance: The permitted timing slack, or `nil` for none.
  /// - Returns: A registration identifying the operation, usable with
  ///   ``OperationExecutor/cancel(_:)`` and
  ///   ``OperationExecutor/escalatePriority(of:to:)``.
  @available(SwiftStdlib 9999, *)
  func enqueue(
    _ job: consuming ExecutorJob,
    at instant: SuspendingClock.Instant,
    tolerance: SuspendingClock.Duration?
  ) -> OperationExecutorRegistration
}

extension Task where Success == Never, Failure == Never {
  /// The ``SuspendingClockExecutor`` that should service a suspending-clock
  /// sleep: the first executor in the current preference chain (active actor
  /// executor, preferred/current task executor) that conforms to
  /// ``SuspendingClockExecutor``, otherwise the default executor.  `nil` only
  /// if even the default executor doesn't implement it.
  @available(StdlibDeploymentTarget 9999, *)
  static var currentSuspendingClockExecutor: (any SuspendingClockExecutor)? {
    _currentOperationExecutor { $0 as? any SuspendingClockExecutor }
  }
}
#endif
