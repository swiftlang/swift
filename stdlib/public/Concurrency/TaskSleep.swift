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
// Sadly we can't easily use Atomic<> from Synchronization here because
// of availability problems.  However, we can use a similar implementation,
// and all we actually need here is a single compare-exchange primitive.
@available(StdlibDeploymentTarget 9999, *)
@_rawLayout(like: SleepCancellationState)
@_staticExclusiveOnly
struct AtomicSleepCancellationState: @unchecked Sendable, ~Copyable {
  var _address: UnsafeMutablePointer<SleepCancellationState> {
    unsafe UnsafeMutablePointer<SleepCancellationState>(_rawAddress)
  }

  var _rawAddress: Builtin.RawPointer {
    return Builtin.addressOfRawLayout(self)
  }

  init(_ initialValue: consuming SleepCancellationState) {
    unsafe _address.initialize(to: initialValue)
  }

  deinit {
    let oldValue = unsafe _address.pointee
    _ = consume oldValue

    unsafe _address.deinitialize(count: 1)
  }

  func compareExchange(expected: SleepCancellationState,
                       desired: SleepCancellationState) -> Bool {
    let (_, swapped) = Builtin.cmpxchg_acqrel_acquire_Int32(
      _rawAddress,
      expected.rawValue._value,
      desired.rawValue._value
    )
    return Bool(swapped)
  }
}

/// A state machine used to make cancellation work correctly; the
/// onCancel: handler can be called concurrently, so there is a race
/// between starting the job and cancellation.
@available(StdlibDeploymentTarget 6.0, *)
enum SleepCancellationState: UInt32 {
  case notStarted
  case started
  case cancelled
}

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
      if let executor = Task.currentSchedulingExecutor {
        await Builtin.suspend(on: executor,
                              until: .after(.nanoseconds(duration)),
                              tolerance: nil,
                              clock: .continuous) { 
          (_: consuming JobCancellationToken) -> () in 
          // Do nothing
        }
        return
      }
    }

    fatalError("No active SchedulingExecutor found")
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
        if let executor = Task.currentSchedulingExecutor {
          let state = AtomicSleepCancellationState(.notStarted)
          nonisolated(unsafe) var token: JobCancellationToken? = nil
          try await withTaskCancellationHandler {
            await Builtin.suspend(on: executor,
                                  until: .after(.nanoseconds(duration)),
                                  tolerance: nil,
                                  clock: .continuous) { 
              (cancellationToken: consuming JobCancellationToken) -> () in

              token = consume cancellationToken

              // If we can't move to the "started" state, we must have hit
              // the cancellation handler *before* the job was enqueued.
              if !state.compareExchange(expected: .notStarted,
                                        desired: .started) {
                if let token {
                  token.cancel()
                }
                token = nil
              }
            }
            return
          } onCancel: {
            // If we can't move to the "cancelled" state directly, it means
            // the job actually started (i.e. the token is valid and we can
            // use it to cancel).
            if !state.compareExchange(expected: .notStarted,
                                      desired: .cancelled) {
              if let token {
                token.cancel()
              }
              token = nil
            }
          }
        } else {
          fatalError("No active SchedulingExecutor found")
        }
      } else {
        fatalError("Unable to sleep because of availability issue")
      }
      
      try Task.checkCancellation()
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
