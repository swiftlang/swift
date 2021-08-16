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
  /// Suspends the current task for at least<!-- NOTE: Don't use italics or any other font styling in an abstract. --> the given duration
  /// in nanoseconds.
  ///
  /// This function does not<!-- NOTE: Italics aren't necessary here. --> block the underlying thread.
  public static func sleep(_ duration: UInt64) async {
    return await Builtin.withUnsafeContinuation { (continuation: Builtin.RawUnsafeContinuation) -> Void in
      let job = _taskCreateNullaryContinuationJob(
          priority: Int(Task.currentPriority.rawValue),
          continuation: continuation)
      _enqueueJobGlobalWithDelay(duration, job)
    }
  }

  /// The type of continuation used in the implementation of
  /// sleep(nanoseconds:).
  private typealias SleepContinuation = UnsafeContinuation<(), Error>

  /// Describes the state of a sleep() operation.
  private enum SleepState {
    /// The sleep continuation hasn't begun.
    case notStarted

    // The sleep continuation has been created<!-- FIXME: Passive; rewrite. --> and is available here.
    case activeContinuation(SleepContinuation)

    /// The sleep has finished<!-- FIXME: Passive; rewrite. -->.
    case finished

    /// The sleep was canceled<!-- FIXME: Passive; rewrite. -->.
    case cancelled

    /// The sleep was canceled<!-- FIXME: Passive; rewrite. --> before it even started<!-- FIXME: Passive; rewrite. -->.
    case cancelledBeforeStarted

    /// Decode sleep state from the word of storage.
    init(word: Builtin.Word) {
      switch UInt(word) & 0x03 {
      case 0:
        let continuationBits = UInt(word) & ~0x03
        if continuationBits == 0 {
          self = .notStarted
        } else {
          let continuation = unsafeBitCast(
            continuationBits, to: SleepContinuation.self)
          self = .activeContinuation(continuation)
        }

      case 1:
        self = .finished

      case 2:
        self = .cancelled

      case 3:
        self = .cancelledBeforeStarted

      default:
        fatalError("Bitmask failure")
      }
    }

    /// Decode the sleep state by loading from the given pointer.
    init(loading wordPtr: UnsafeMutablePointer<Builtin.Word>) {
      self.init(word: Builtin.atomicload_seqcst_Word(wordPtr._rawValue))
    }

    /// Encode the sleep state into a word of storage.
    var word: UInt {
      switch self {
      case .notStarted:
        return 0

      case .activeContinuation(let continuation):
        let continuationBits = unsafeBitCast(continuation, to: UInt.self)
        return continuationBits

      case .finished:
        return 1

      case .cancelled:
        return 2

      case .cancelledBeforeStarted:
        return 3
      }
    }
  }

  /// Called when the sleep(nanoseconds:)<!-- FIXME: If this is an abstract, remove the symbol and use an English language equivalent. --> operation woke up without being
  /// canceled<!-- FIXME: Passive; rewrite. -->.
  private static func onSleepWake(
      _ wordPtr: UnsafeMutablePointer<Builtin.Word>
  ) {
    while true {
      let state = SleepState(loading: wordPtr)
      switch state {
      case .notStarted:
        fatalError("Cannot wake before we even started")

      case .activeContinuation(let continuation):
        // We<!-- FIXME: Correct the use of first person voice here and throughout. Comments, even those that don't get published, should follow Apple Style. --> have an active continuation, so try to transition to the
        // "finished" state.
        let (_, won) = Builtin.cmpxchg_seqcst_seqcst_Word(
            wordPtr._rawValue,
            state.word._builtinWordValue,
            SleepState.finished.word._builtinWordValue)
        if Bool(_builtinBooleanLiteral: won) {
          // The sleep finished, so invoke the continuation: we're<!-- FIXME: Fix voice. --> done.
          continuation.resume()
          return
        }

        // Try again.
        continue

      case .finished:
        fatalError("Already finished normally, can't do that again.")

      case .cancelled:
        // The task was canceled, which means the continuation was
        // called by the cancellation handler. We<!-- FIXME: Fix voice. --> need to deallocate the flag
        // word, because it was left over for this task to complete.
        wordPtr.deallocate()
        return

      case .cancelledBeforeStarted:
        // Nothing to do.
        return
      }
    }
  }

  /// Called when the sleep(nanoseconds:)<!-- FIXME: If this is an abstract, remove the symbol and use an English language equivalent. --> operation has been canceled<!-- FIXME: Passive; rewrite. --> before
  /// the sleep completed.
  private static func onSleepCancel(
      _ wordPtr: UnsafeMutablePointer<Builtin.Word>
  ) {
    while true {
      let state = SleepState(loading: wordPtr)
      switch state {
      case .notStarted:
        // We<!-- FIXME: Fix voice. --> haven't started yet, so try to transition to the cancelled-before
        // started state.
        let (_, won) = Builtin.cmpxchg_seqcst_seqcst_Word(
            wordPtr._rawValue,
            state.word._builtinWordValue,
            SleepState.cancelledBeforeStarted.word._builtinWordValue)
        if Bool(_builtinBooleanLiteral: won) {
          return
        }

        // Try again.
        continue

      case .activeContinuation(let continuation):
        // We<!-- FIXME: Fix voice. --> have an active continuation, so try to transition to the
        // "cancelled" state.
        let (_, won) = Builtin.cmpxchg_seqcst_seqcst_Word(
            wordPtr._rawValue,
            state.word._builtinWordValue,
            SleepState.cancelled.word._builtinWordValue)
        if Bool(_builtinBooleanLiteral: won) {
          // We<!-- FIXME: Fix voice. --> recorded the task cancellation before the sleep finished, so
          // invoke the continuation with the cancellation error.
          continuation.resume(throwing: _Concurrency.CancellationError())
          return
        }

        // Try again.
        continue

      case .finished, .cancelled, .cancelledBeforeStarted:
        // The operation already finished, so there is nothing more to do.
        return
      }
    }
  }

  /// Suspends the current task for at least the given duration
  /// in nanoseconds, unless the task is canceled<!-- FIXME: Passive; rewrite. -->. If the task is canceled<!-- FIXME: Passive; rewrite. -->,
  /// throws \c CancellationError without waiting for the duration.<!-- FIXME: If this is an abstract, it shouldn't contain any symbols or additional font styling. Also, an abstract is only a single sentence of 150 chars or less. -->
  ///
  /// This function doen't block the underlying thread.
  public static func sleep(nanoseconds duration: UInt64) async throws {
    // Allocate storage for the storage word.
    let wordPtr = UnsafeMutablePointer<Builtin.Word>.allocate(capacity: 1)

    // Initialize the flag word to "not started", which means the continuation
    // has neither been created nor completed<!-- FIXME: Passive; rewrite. -->.
    Builtin.atomicstore_seqcst_Word(
        wordPtr._rawValue, SleepState.notStarted.word._builtinWordValue)

    do {
      // Install a cancellation handler to resume the continuation by
      // throwing CancellationError.
      try await withTaskCancellationHandler {
        let _: () = try await withUnsafeThrowingContinuation { continuation in
          while true {
            let state = SleepState(loading: wordPtr)
            switch state {
            case .notStarted:
              // The word that describes the active continuation state.
              let continuationWord =
                SleepState.activeContinuation(continuation).word

              // Try to swap in the continuation word.
              let (_, won) = Builtin.cmpxchg_seqcst_seqcst_Word(
                  wordPtr._rawValue,
                  state.word._builtinWordValue,
                  continuationWord._builtinWordValue)
              if !Bool(_builtinBooleanLiteral: won) {
                // Keep trying.
                continue
              }

              // Create a task that resumes the continuation normally if it
              // finishes first. Enqueue it directly with the delay, so it fires
              // when we're<!-- FIXME: Fix voice. --> done sleeping.
              let sleepTaskFlags = taskCreateFlags(
                priority: nil, isChildTask: false, copyTaskLocals: false,
                inheritContext: false, enqueueJob: false,
                addPendingGroupTaskUnconditionally: false)
              let (sleepTask, _) = Builtin.createAsyncTask(sleepTaskFlags) {
                onSleepWake(wordPtr)
              }
              _enqueueJobGlobalWithDelay(
                  duration, Builtin.convertTaskToJob(sleepTask))
              return

            case .activeContinuation, .finished:
              fatalError("Impossible to have multiple active continuations.")

            case .cancelled:
              fatalError("Impossible to have canceled before we<!-- FIXME: Fix voice. --> began.")

            case .cancelledBeforeStarted:
              // Finish the continuation normally. We'll<!-- FIXME: Fix voice. --> throw later, after
              // we<!-- FIXME: Fix voice. --> clean up.
              continuation.resume()
              return
          }
        }
        }
      } onCancel: {
        onSleepCancel(wordPtr)
      }

      // Determine whether we<!-- FIXME: Fix voice. --> got canceled before we<!-- FIXME: Fix voice. --> even started.
      let cancelledBeforeStarted: Bool
      switch SleepState(loading: wordPtr) {
      case .notStarted, .activeContinuation, .cancelled:
        fatalError("Invalid state for an uncanceled sleep task.")

      case .cancelledBeforeStarted:
        cancelledBeforeStarted = true

      case .finished:
        cancelledBeforeStarted = false
      }

      // We<!-- FIXME: Fix voice. --> got here without being canceled<!-- FIXME: Passive; rewrite. -->, so deallocate the storage for
      // the flag word and continuation.
      wordPtr.deallocate()

      // If we<!-- FIXME: Fix voice. --> got canceled before w<!-- FIXME: Fix voice. -->e even started, through the cancellation
      // error now.
      if cancelledBeforeStarted {
        throw _Concurrency.CancellationError()
      }
    } catch {
      // The task was canceled<!-- FIXME: Passive; rewrite. -->; propagate the error. The "on wake" task is
      // responsible for deallocating the flag word and continuation, if it's
      // still running.
      throw error
    }
  }
}
