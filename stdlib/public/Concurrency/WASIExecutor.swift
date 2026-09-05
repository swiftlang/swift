//===--- WASIExecutor.swift -----------------------------------------------===//
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
//
// The executors for `wasm32-unknown-wasip1-threads`.
//
// Unlike the single-threaded WebAssembly build (which uses the cooperative
// executor), the threads triple has shared memory, atomics and `thread_spawn`,
// so the default executor fans `Task`, `TaskGroup` and `async let` work out
// across a pool of worker threads. These types are thin wrappers over the C
// helpers in `WASIGlobalExecutor.cpp` (declared in ExecutorBridge.swift),
// mirroring how `DispatchExecutor` wraps libdispatch on Darwin and Linux.
//
//===----------------------------------------------------------------------===//

// Only compiled for the `wasi` global executor (stdlib/public/Concurrency/
// CMakeLists.txt); the guard keeps a stray build from producing an empty
// module with no `PlatformExecutorFactory`.
#if os(WASI)

import Swift

// .. Main executor ...........................................................

/// The main-thread executor: a serial queue drained by the thread that runs
/// the async `main`. Worker threads wake it when they resume a `@MainActor`
/// continuation.
@available(StdlibDeploymentTarget 6.3, *)
final class WASIMainExecutor: RunLoopExecutor, @unchecked Sendable {
  var running = false

  public init() {}

  /// Drain the main queue forever. The loop lives here rather than in C so
  /// every job runs with this executor as the active one (a `@MainActor`
  /// function calling a `nonisolated async` function then hops to the pool
  /// instead of running inline, and main-actor isolation checks take the
  /// executor fast path).
  public func run() throws {
    if running {
      fatalError("WASIMainExecutor does not support recursion")
    }
    running = true
    while true {
      let job = unsafe _wasiWaitForMainJob()
      unsafe ExecutorJob(job).runSynchronously(on: asUnownedSerialExecutor())
    }
  }

  public func stop() {
    fatalError("WASIMainExecutor cannot be stopped")
  }
}

@available(StdlibDeploymentTarget 6.3, *)
extension WASIMainExecutor: SerialExecutor {
  public func enqueue(_ job: consuming ExecutorJob) {
    _wasiEnqueueMain(UnownedJob(job))
  }

  public func isIsolatingCurrentContext() -> Bool? {
    // The threads triple builds with the pthreads threading package, so the
    // runtime can tell the main thread apart from a pool worker.
    _wasiIsMainThread()
  }

  public func checkIsolated() {
    if !_wasiIsMainThread() {
      fatalError("Incorrect actor executor assumption; expected the main "
                 + "executor, but this code is running on a WASI thread-pool "
                 + "worker")
    }
  }
}

@available(StdlibDeploymentTarget 6.3, *)
extension WASIMainExecutor: MainExecutor {}

// .. Global (default) executor ...............................................

/// The concurrent default executor: jobs are handed to a pool of wasi-threads
/// worker threads (`SWIFT_WASI_EXECUTOR_THREADS` overrides the pool size).
@available(StdlibDeploymentTarget 6.3, *)
final class WASIGlobalExecutor: TaskExecutor, SchedulingExecutor,
                                @unchecked Sendable {
  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    _wasiEnqueueGlobal(UnownedJob(job))
  }

  // The runtime schedules through the `after:` form (`enqueue(_:at:...)` is
  // derived by the protocol default); `tolerance` is accepted but not used —
  // the pool wakes at the deadline.
  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    let (clockID, seconds, nanoseconds) = _wasiDelayComponents(delay, clock: clock)
    _wasiEnqueueGlobalWithDelay(CLongLong(seconds), CLongLong(nanoseconds),
                                clockID.rawValue, UnownedJob(job))
  }
}

/// The delay as (seconds, nanoseconds) on a clock the backend knows.
@available(StdlibDeploymentTarget 6.3, *)
fileprivate func _wasiDelayComponents<C: Clock>(_ delay: C.Duration, clock: C)
  -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64) {
  if let continuousClock = clock as? ContinuousClock {
    let (seconds, nanoseconds) =
      continuousClock.durationComponents(for: delay as! ContinuousClock.Duration)
    return (.continuous, seconds, nanoseconds)
  } else if let suspendingClock = clock as? SuspendingClock {
    let (seconds, nanoseconds) =
      suspendingClock.durationComponents(for: delay as! SuspendingClock.Duration)
    return (.suspending, seconds, nanoseconds)
  }
  fatalError("Sorry, cannot schedule on an unknown clock")
}

#endif // os(WASI)
