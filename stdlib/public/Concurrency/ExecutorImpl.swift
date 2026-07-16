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
// Implementations of the *Impl functions that bridge to Swift.
//
// These are separate from the functions in ExecutorBridge.swift so that we
// can omit this file from the Embedded Swift version of Concurrency for now.
// (This means that the existing co-operative executor will still work.)
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_asyncMainDrainQueueImpl")
@diagnose(UselessAvailabilityCheck, as: ignored)
internal func drainMainQueue() {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  if #available(StdlibDeploymentTarget 6.3, *) {
    try! MainActor.executor.run()
    _exit(result: 0)
  } else {
    fatalError("this should never happen")
  }
  #else
  fatalError("swift_task_asyncMainDrainQueue() not supported with task-to-thread")
  #endif
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_donateThreadToGlobalExecutorUntilImpl")
@diagnose(UselessAvailabilityCheck, as: ignored)
internal func donateToGlobalExecutor(
  condition: @convention(c) (_ ctx: UnsafeMutableRawPointer) -> CBool,
  context: UnsafeMutableRawPointer
) {
  if #available(StdlibDeploymentTarget 6.3, *) {
    if let runnableExecutor = Task.defaultExecutor as? RunLoopExecutor {
      try! runnableExecutor.runUntil { unsafe Bool(condition(context)) }
    } else {
      fatalError("Global executor does not support thread donation")
    }
  } else {
    fatalError("this should never happen")
  }
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_getMainExecutorImpl")
@diagnose(UselessAvailabilityCheck, as: ignored)
internal func getMainExecutor() -> UnownedSerialExecutor {
  if #available(StdlibDeploymentTarget 6.3, *) {
    return unsafe _getMainExecutorAsSerialExecutor()
  } else {
    fatalError("this should never happen")
  }
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueMainExecutorImpl")
@diagnose(UselessAvailabilityCheck, as: ignored)
@diagnose(DeprecatedDeclaration, as: ignored)
internal func enqueueOnMainExecutor(job unownedJob: UnownedJob) {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  if #available(StdlibDeploymentTarget 6.3, *) {
    MainActor.executor.enqueue(unownedJob)
  } else {
    fatalError("this should never happen")
  }
  #else
  fatalError("swift_task_enqueueMainExecutor() not supported for task-to-thread")
  #endif
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalImpl")
@diagnose(UselessAvailabilityCheck, as: ignored)
internal func enqueueOnGlobalExecutor(job unownedJob: UnownedJob) {
  if #available(StdlibDeploymentTarget 6.3, *) {
    Task.defaultExecutor.enqueue(unownedJob)
  } else {
    fatalError("this should never happen")
  }
}

#if !$Embedded
// Global job scheduling with a delay/deadline is not supported by the
// continuation-native executor.  Time-based suspension is now expressed through
// the clock executors' *continuation* `enqueue` (see `Clock.sleep`).  These
// legacy runtime hooks remain only for ABI stability; a future design may
// reintroduce job-based deadline scheduling via the clock executors' job
// `enqueue` requirement.
@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalWithDelayImpl")
internal func enqueueOnGlobalExecutor(delay: CUnsignedLongLong,
                                      job unownedJob: UnownedJob) {
  fatalError("swift_task_enqueueGlobalWithDelay is not supported")
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalWithDeadlineImpl")
internal func enqueueOnGlobalExecutor(seconds: CLongLong,
                                      nanoseconds: CLongLong,
                                      leewaySeconds: CLongLong,
                                      leewayNanoseconds: CLongLong,
                                      clock: CInt,
                                      job unownedJob: UnownedJob) {
  fatalError("swift_task_enqueueGlobalWithDeadline is not supported")
}
#endif
