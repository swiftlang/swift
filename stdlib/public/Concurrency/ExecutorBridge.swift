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

@available(SwiftStdlib 6.2, *)
@_silgen_name("_swift_exit")
internal func _exit(result: CInt)

#if !$Embedded
@available(SwiftStdlib 6.2, *)
@_silgen_name("_swift_task_isMainExecutorSwift")
internal func _isMainExecutor<E>(_ executor: E) -> Bool where E: SerialExecutor {
  return executor.isMainExecutor
}
#endif

@available(SwiftStdlib 6.2, *)
@_silgen_name("_swift_task_checkIsolatedSwift")
internal func checkIsolated<E>(executor: E) where E: SerialExecutor {
  executor.checkIsolated()
}

/// Invokes the swift function isIsolatingCurrentContext on the given executor,
/// and converts between the `Optional<Bool>` into:
///     -1: unknown
///      0: not isolated
///      1: isolated
@available(SwiftStdlib 6.2, *)
@_silgen_name("_swift_task_isIsolatingCurrentContextSwift")
internal func isIsolatingCurrentContext<E>(executor: E) -> Int8
  where E: SerialExecutor {
  switch executor.isIsolatingCurrentContext() {
  case nil: -1 // unknown
  case .some(false): 0 // not isolated
  case .some(true): 1 // isolated!
  }
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("_swift_getActiveExecutor")
internal func _getActiveExecutor() -> UnownedSerialExecutor

@available(SwiftStdlib 6.2, *)
@_silgen_name("_swift_getCurrentTaskExecutor")
internal func _getCurrentTaskExecutor() -> UnownedTaskExecutor

@available(SwiftStdlib 6.2, *)
@_silgen_name("_swift_getPreferredTaskExecutor")
internal func _getPreferredTaskExecutor() -> UnownedTaskExecutor

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_job_allocate")
internal func _jobAllocate(_ job: Builtin.Job,
                           _ capacity: Int) -> UnsafeMutableRawPointer

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_job_deallocate")
internal func _jobDeallocate(_ job: Builtin.Job,
                             _ address: UnsafeMutableRawPointer)

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_job_getPriority")
internal func _jobGetPriority(_ job: Builtin.Job) -> UInt8

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_job_getKind")
internal func _jobGetKind(_ job: Builtin.Job) -> UInt8

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_job_getExecutorPrivateData")
internal func _jobGetExecutorPrivateData(
  _ job: Builtin.Job
) -> UnsafeMutableRawPointer

#if !$Embedded
#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_getMainExecutor")
internal func _getMainExecutor() -> any SerialExecutor {
  return MainActor.executor
}
#else
// For task-to-thread model, this is implemented in C++
@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_getMainExecutor")
internal func _getMainExecutor() -> any SerialExecutor
#endif // SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
#endif // !$Embedded

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_dispatchMain")
internal func _dispatchMain()

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_dispatchEnqueueMain")
internal func _dispatchEnqueueMain(_ job: UnownedJob)

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_dispatchEnqueueGlobal")
internal func _dispatchEnqueueGlobal(_ job: UnownedJob)

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_dispatchEnqueueWithDeadline")
internal func _dispatchEnqueueWithDeadline(_ global: CBool,
                                           _ sec: CLongLong,
                                           _ nsec: CLongLong,
                                           _ tsec: CLongLong,
                                           _ tnsec: CLongLong,
                                           _ clock: CInt,
                                           _ job: UnownedJob)

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_dispatchAssertMainQueue")
internal func _dispatchAssertMainQueue()

@_silgen_name("swift_getDispatchQueueForExecutor")
internal func _getDispatchQueueForExecutor(
  _ executor: UnownedSerialExecutor
) -> OpaquePointer?

@_silgen_name("swift_createDefaultExecutorsOnce")
func _createDefaultExecutorsOnce()
