//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Functions to bridge between C++ and Swift.  This does not include the
// *Impl functions because we need them separate for Embedded Swift.
//
//===----------------------------------------------------------------------===//

import Swift

@available(StdlibDeploymentTarget 6.3, *)
@_extern(c, "_swift_exit")
internal func _exit(result: CInt)

#if !$Embedded
@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("_swift_task_isMainExecutorSwift")
internal func _isMainExecutor<E>(_ executor: E) -> Bool where E: SerialExecutor {
  return executor.isMainExecutor
}
#endif

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("_swift_task_checkIsolatedSwift")
internal func checkIsolated<E>(executor: E) where E: SerialExecutor {
  executor.checkIsolated()
}

/// Invokes the swift function isIsolatingCurrentContext on the given executor,
/// and converts between the `Optional<Bool>` into:
///     -1: unknown
///      0: not isolated
///      1: isolated
@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("_swift_task_isIsolatingCurrentContextSwift")
internal func isIsolatingCurrentContext<E>(executor: E) -> Int8
  where E: SerialExecutor {
  switch executor.isIsolatingCurrentContext() {
  case nil: -1 // unknown
  case .some(false): 0 // not isolated
  case .some(true): 1 // isolated!
  }
}

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("_swift_getActiveExecutor")
internal func _getActiveExecutor() -> UnownedSerialExecutor

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("_swift_getCurrentTaskExecutor")
internal func _getCurrentTaskExecutor() -> UnownedTaskExecutor

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("_swift_getPreferredTaskExecutor")
internal func _getPreferredTaskExecutor() -> UnownedTaskExecutor

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_job_allocate")
internal func _jobAllocate(_ job: Builtin.Job,
                           _ capacity: Int) -> UnsafeMutableRawPointer

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_job_deallocate")
internal func _jobDeallocate(_ job: Builtin.Job,
                             _ address: UnsafeMutableRawPointer)

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_job_getPriority")
internal func _jobGetPriority(_ job: Builtin.Job) -> UInt8

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_job_setPriority")
internal func _jobSetPriority(_ job: Builtin.Job, _ priority: UInt8)

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_job_getKind")
internal func _jobGetKind(_ job: Builtin.Job) -> UInt8

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_job_getExecutorPrivateData")
internal func _jobGetExecutorPrivateData(
  _ job: Builtin.Job
) -> UnsafeMutableRawPointer

#if os(WASI) || os(Emscripten) || !$Embedded
#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_getMainExecutor")
internal func _getMainExecutorAsSerialExecutor() -> UnownedSerialExecutor {
  return unsafe MainActor.unownedExecutor
}
#else
// For task-to-thread model, this is implemented in C++
@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_getMainExecutor")
internal func _getMainExecutorAsSerialExecutor() -> UnownedSerialExecutor
#endif // SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
#endif // os(WASI) || os(Emscripten) || !$Embedded

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_getDefaultExecutor")
internal func _getDefaultExecutorAsTaskExecutor() -> UnownedTaskExecutor {
  return unsafe Task.unownedDefaultExecutor
}

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_dispatchMain")
internal func _dispatchMain()

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_dispatchEnqueueMain")
internal func _dispatchEnqueueMain(_ job: UnownedJob)

@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_dispatchEnqueueGlobal")
internal func _dispatchEnqueueGlobal(_ job: UnownedJob)

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
// A per-clock dispatch timer whose scheduling state is owned entirely by the
// Swift `DispatchExecutor`.  `fire` is a C-convention trampoline invoked on the
// timer queue when the armed deadline elapses.
@available(StdlibDeploymentTarget 9999, *)
@_silgen_name("swift_dispatchTimerCreate")
internal func _dispatchTimerCreate(
  _ clock: CInt,
  _ qos: CInt,
  _ fire: @convention(c) (UnsafeMutableRawPointer?) -> Void,
  _ context: UnsafeMutableRawPointer?
) -> UnsafeMutableRawPointer?

@available(StdlibDeploymentTarget 9999, *)
@_silgen_name("swift_dispatchTimerSet")
internal func _dispatchTimerSet(_ timer: UnsafeMutableRawPointer?,
                                _ sec: CLongLong,
                                _ nsec: CLongLong,
                                _ leewaySec: CLongLong,
                                _ leewayNsec: CLongLong)

@available(StdlibDeploymentTarget 9999, *)
@_silgen_name("swift_dispatchTimerDisarm")
internal func _dispatchTimerDisarm(_ timer: UnsafeMutableRawPointer?)
#endif


@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_dispatchAssertMainQueue")
internal func _dispatchAssertMainQueue()

@_silgen_name("swift_createDefaultExecutorsOnce")
func _createDefaultExecutorsOnce()

@_silgen_name("swift_getDispatchQueueForExecutor")
internal func _getDispatchQueueForExecutor(
  _ executor: UnownedSerialExecutor
) -> OpaquePointer?

// Resume a suspended task from a continuation, running it inline on the current
// thread.
@available(StdlibDeploymentTarget 9999, *)
@usableFromInline
@_silgen_name("swift_continuation_resumeSynchronously")
internal func _swiftContinuationResumeSynchronously(
  _ continuation: Builtin.RawUnsafeContinuation
)

// Resume a suspended task from a continuation by throwing an error, running it
// inline on the current thread.
@available(StdlibDeploymentTarget 9999, *)
@usableFromInline
@_silgen_name("swift_continuation_throwingResumeSynchronouslyWithError")
internal func _swiftContinuationThrowingResumeSynchronouslyWithError(
  _ continuation: Builtin.RawUnsafeContinuation,
  _ error: __owned any Error
)
