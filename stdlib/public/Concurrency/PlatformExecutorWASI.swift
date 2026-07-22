//===--- PlatformExecutorWASI.swift ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The default executors for `wasm32-unknown-wasip1-threads`.
//
// Unlike the single-threaded WebAssembly build (which uses the cooperative
// executor), the threads triple has shared memory, atomics and `thread_spawn`,
// so the global/default executor can fan `Task`, `TaskGroup` and `async let`
// work out across a pool of worker threads. These types are thin wrappers over
// the C helpers in `WASIGlobalExecutor.cpp`, mirroring how `DispatchExecutor`
// wraps libdispatch on Darwin and Linux.
//
//===----------------------------------------------------------------------===//

// This file is only added to the build for the wasi-threads triple (see the
// `wasi` branch of the `SWIFT_CONCURRENCY_GLOBAL_EXECUTOR` selection in
// `CMakeLists.txt`); the `os(WASI)` guard is belt-and-suspenders.
#if os(WASI)

import Swift

// .. Main executor ...........................................................

/// The main-thread executor: a serial queue drained by the thread that runs
/// the async `main`. Worker threads wake it when they resume a `@MainActor`
/// continuation.
@available(StdlibDeploymentTarget 6.3, *)
final class WASIMainExecutor: RunLoopExecutor, @unchecked Sendable {
  public init() {}

  public func run() throws {
    _wasiDrainMain()
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

  public func checkIsolated() {
    // The runtime performs its own main-actor isolation checking; there is no
    // cheap "am I the main thread" probe to assert against on WASI.
  }
}

@available(StdlibDeploymentTarget 6.3, *)
extension WASIMainExecutor: MainExecutor {}

// .. Global (default) executor ...............................................

/// The concurrent default executor: jobs are handed to a pool of wasi-threads
/// worker threads.
@available(StdlibDeploymentTarget 6.3, *)
final class WASIGlobalExecutor: TaskExecutor, SchedulingExecutor,
                                @unchecked Sendable {
  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    _wasiEnqueueGlobal(UnownedJob(job))
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                at instant: C.Instant,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    // Schedule relative to the supplied clock; the backend only needs the delay
    // in nanoseconds (it waits on a monotonic deadline).
    let delay = clock.now.duration(to: instant)
    _wasiEnqueueGlobalWithDelay(_nanoseconds(from: delay, clock: clock),
                                UnownedJob(job))
  }
}

@available(StdlibDeploymentTarget 6.3, *)
fileprivate func _nanoseconds<C: Clock>(from duration: C.Duration,
                                        clock: C) -> Int64 {
  let swiftDuration: Swift.Duration
  if clock is ContinuousClock {
    swiftDuration = duration as! ContinuousClock.Duration
  } else if clock is SuspendingClock {
    swiftDuration = duration as! SuspendingClock.Duration
  } else {
    // Best effort for other clocks: treat the duration as already in seconds.
    return 0
  }
  let (seconds, attoseconds) = swiftDuration.components
  if seconds < 0 || (seconds == 0 && attoseconds < 0) {
    return 0
  }
  let ns = seconds &* 1_000_000_000 &+ (attoseconds / 1_000_000_000)
  return ns
}

// .. Factory .................................................................

@_spi(ExperimentalCustomExecutors)
@available(StdlibDeploymentTarget 6.3, *)
public struct PlatformExecutorFactory: ExecutorFactory {
  public static let mainExecutor: any MainExecutor = WASIMainExecutor()
  public static let defaultExecutor: any TaskExecutor = WASIGlobalExecutor()
}

// .. C runtime helpers (WASIGlobalExecutor.cpp) ..............................

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_wasiEnqueueGlobal")
internal func _wasiEnqueueGlobal(_ job: UnownedJob)

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_wasiEnqueueGlobalWithDelay")
internal func _wasiEnqueueGlobalWithDelay(_ delayNanoseconds: Int64,
                                          _ job: UnownedJob)

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_wasiEnqueueMain")
internal func _wasiEnqueueMain(_ job: UnownedJob)

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_wasiDrainMain")
internal func _wasiDrainMain()

#endif // os(WASI)
