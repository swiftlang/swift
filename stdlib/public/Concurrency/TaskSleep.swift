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
@_implementationOnly import _SwiftConcurrencyShims

@available(SwiftStdlib 5.5, *)
extension Task where Success == Never, Failure == Never {
  /// Suspends the current task for _at least_ the given duration
  /// in nanoseconds.
  ///
  /// This function does _not_ block the underlying thread.
  public static func sleep(_ duration: UInt64) async {
    let currentTask = Builtin.getCurrentAsyncTask()
    let priority = getJobFlags(currentTask).priority ?? Task.currentPriority._downgradeUserInteractive

    return await Builtin.withUnsafeContinuation { (continuation: Builtin.RawUnsafeContinuation) -> Void in
      let job = _taskCreateNullaryContinuationJob(priority: Int(priority.rawValue), continuation: continuation)
      _enqueueJobGlobalWithDelay(duration, job)
    }
  }

  /// The type of continuation used in the implementation of
  /// sleep(nanoseconds:).
  private typealias SleepContinuation = UnsafeContinuation<(), Error>

  /// Called when the sleep(nanoseconds:) operation woke up without being
  /// cancelled.
  private static func onSleepWake(
      _ wordPtr: UnsafeMutablePointer<Builtin.Word>,
      _ continuation: UnsafeContinuation<(), Error>
  ) {
    // Indicate that we've finished by putting a "1" into the flag word.
    let (_, won) = Builtin.cmpxchg_seqcst_seqcst_Word(
        wordPtr._rawValue,
        UInt(0)._builtinWordValue,
        UInt(1)._builtinWordValue)

    if Bool(_builtinBooleanLiteral: won) {
      // The sleep finished, invoke the continuation.
      continuation.resume()
    } else {
      // The task was cancelled first, which means the continuation was
      // called by the cancellation handler. We need to deallocate up the flag
      // word, because it was left over for this task to complete.
      wordPtr.deallocate()
    }
  }

  /// Called when the sleep(nanoseconds:) operation has been cancelled before
  /// the sleep completed.
  private static func onSleepCancel(
      _ wordPtr: UnsafeMutablePointer<Builtin.Word>,
      _ continuation: UnsafeContinuation<(), Error>
  ) {
    // Indicate that we've finished by putting a "2" into the flag word.
    let (_, won) = Builtin.cmpxchg_seqcst_seqcst_Word(
        wordPtr._rawValue,
        UInt(0)._builtinWordValue,
        UInt(2)._builtinWordValue)

    if Bool(_builtinBooleanLiteral: won) {
      // We recorded the task cancellation before the sleep finished, so
      // invoke the continuation with a the cancellation error.
      continuation.resume(throwing: _Concurrency.CancellationError())
    }
  }

  /// Suspends the current task for _at least_ the given duration
  /// in nanoseconds, unless the task is cancelled. If the task is cancelled,
  /// throws \c CancellationError without waiting for the duration.
  ///
  /// This function does _not_ block the underlying thread.
  public static func sleep(nanoseconds duration: UInt64) async throws {
    // If the task was already cancelled, go ahead and throw now.
    try checkCancellation()

    // Allocate storage for the flag word and continuation.
    let wordPtr = UnsafeMutablePointer<Builtin.Word>.allocate(capacity: 2)

    // Initialize the flag word to 0, which means the continuation has not
    // executed.
    Builtin.atomicstore_seqcst_Word(
        wordPtr._rawValue, UInt(0)._builtinWordValue)

    // A pointer to the storage continuation. Also initialize it to zero, to
    // indicate that there is no continuation.
    let continuationPtr = wordPtr + 1
    Builtin.atomicstore_seqcst_Word(
      continuationPtr._rawValue, UInt(0)._builtinWordValue)

    do {
      // Install a cancellation handler to resume the continuation by
      // throwing CancellationError.
      try await withTaskCancellationHandler {
        let _: () = try await withUnsafeThrowingContinuation { continuation in
          // Stash the continuation so the cancellation handler can see it.
          Builtin.atomicstore_seqcst_Word(
            continuationPtr._rawValue,
            unsafeBitCast(continuation, to: Builtin.Word.self))

          // Create a task that resumes the continuation normally if it
          // finishes first. Enqueue it directly with the delay, so it fires
          // when we're done sleeping.
          let sleepTaskFlags = taskCreateFlags(
            priority: nil, isChildTask: false, copyTaskLocals: false,
            inheritContext: false, enqueueJob: false,
            addPendingGroupTaskUnconditionally: false)
          let (sleepTask, _) = Builtin.createAsyncTask(sleepTaskFlags) {
            onSleepWake(wordPtr, continuation)
          }
          _enqueueJobGlobalWithDelay(
              duration, Builtin.convertTaskToJob(sleepTask))
        }
      } onCancel: {
        let continuationWord = continuationPtr.pointee
        if UInt(continuationWord) != 0 {
          // Try to cancel, which will resume the continuation by throwing a
          // CancellationError if the continuation hasn't already been resumed.
          continuationPtr.withMemoryRebound(
              to: SleepContinuation.self, capacity: 1) {
            onSleepCancel(wordPtr, $0.pointee)
          }
        }
      }

      // We got here without being cancelled, so deallocate the storage for
      // the flag word and continuation.
      wordPtr.deallocate()
    } catch {
      // The task was cancelled; propagate the error. The "on wake" task is
      // responsible for deallocating the flag word.
      throw error
    }
  }
}
