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
internal func drainMainQueue() {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  try! MainActor.executor.run()
  _exit(result: 0)
  #else
  fatalError("swift_task_asyncMainDrainQueue() not supported with task-to-thread")
  #endif
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_donateThreadToGlobalExecutorUntilImpl")
internal func donateToGlobalExecutor(
  condition: @convention(c) (_ ctx: UnsafeMutableRawPointer) -> CBool,
  context: UnsafeMutableRawPointer
) {
  if let runnableExecutor = Task.defaultExecutor as? RunLoopExecutor {
    try! runnableExecutor.runUntil { unsafe Bool(condition(context)) }
  } else {
    fatalError("Global executor does not support thread donation")
  }
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_getMainExecutorImpl")
internal func getMainExecutor() -> UnownedSerialExecutor {
  let executor = _getMainExecutorAsSerialExecutor()
  if let executor {
    return unsafe executor.asUnownedSerialExecutor()
  }
  return unsafe unsafeBitCast(executor, to: UnownedSerialExecutor.self)
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueMainExecutorImpl")
internal func enqueueOnMainExecutor(job unownedJob: UnownedJob) {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  MainActor.executor.enqueue(unownedJob)
  #else
  fatalError("swift_task_enqueueMainExecutor() not supported for task-to-thread")
  #endif
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalImpl")
internal func enqueueOnGlobalExecutor(job unownedJob: UnownedJob) {
  Task.defaultExecutor.enqueue(unownedJob)
}

#if !$Embedded
@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalWithDelayImpl")
internal func enqueueOnGlobalExecutor(delay: CUnsignedLongLong,
                                      job unownedJob: UnownedJob) {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  Task.defaultExecutor.asSchedulable!.enqueue(ExecutorJob(unownedJob),
                                              after: .nanoseconds(delay),
                                              clock: .continuous)
  #else
  fatalError("swift_task_enqueueGlobalWithDelay() not supported for task-to-thread")
  #endif
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalWithDeadlineImpl")
internal func enqueueOnGlobalExecutor(seconds: CLongLong,
                                      nanoseconds: CLongLong,
                                      leewaySeconds: CLongLong,
                                      leewayNanoseconds: CLongLong,
                                      clock: CInt,
                                      job unownedJob: UnownedJob) {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  let delay = Duration.seconds(seconds) + Duration.nanoseconds(nanoseconds)
  let leeway = Duration.seconds(leewaySeconds) + Duration.nanoseconds(leewayNanoseconds)
  switch clock {
    case _ClockID.suspending.rawValue:
      Task.defaultExecutor.asSchedulable!.enqueue(ExecutorJob(unownedJob),
                                                  after: delay,
                                                  tolerance: leeway,
                                                  clock: .suspending)
    case _ClockID.continuous.rawValue:
      Task.defaultExecutor.asSchedulable!.enqueue(ExecutorJob(unownedJob),
                                                  after: delay,
                                                  tolerance: leeway,
                                                  clock: .continuous)
    default:
      fatalError("Unknown clock ID \(clock)")
  }
  #else
  fatalError("swift_task_enqueueGlobalWithDeadline() not supported for task-to-thread")
  #endif
}
#endif
