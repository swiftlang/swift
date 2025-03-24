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

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_asyncMainDrainQueueImpl")
internal func drainMainQueue() {
  try! MainActor.executor.run()
  _exit(result: 0)
}
#endif

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_donateThreadToGlobalExecutorUntilImpl")
internal func dontateToGlobalExecutor(
  condition: @convention(c) (_ ctx: UnsafeMutableRawPointer) -> CBool,
  context: UnsafeMutableRawPointer
) {
  if let runnableExecutor = Task.defaultExecutor as? RunLoopExecutor {
    try! runnableExecutor.runUntil { unsafe Bool(condition(context)) }
  } else {
    fatalError("Global executor does not support thread donation")
  }
}

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_getMainExecutorImpl")
internal func getMainExecutor() -> UnownedSerialExecutor {
  return unsafe UnownedSerialExecutor(MainActor.executor)
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueMainExecutorImpl")
internal func enqueueOnMainExecutor(job unownedJob: UnownedJob) {
  MainActor.executor.enqueue(unownedJob)
}
#endif

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalImpl")
internal func enqueueOnGlobalExecutor(job unownedJob: UnownedJob) {
  Task.defaultExecutor.enqueue(unownedJob)
}

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalWithDelayImpl")
internal func enqueueOnGlobalExecutor(delay: CUnsignedLongLong,
                                      job unownedJob: UnownedJob) {
  Task.defaultExecutor.asSchedulable!.enqueue(ExecutorJob(unownedJob),
                                              after: .nanoseconds(delay),
                                              clock: .continuous)
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalWithDeadlineImpl")
internal func enqueueOnGlobalExecutor(seconds: CLongLong,
                                      nanoseconds: CLongLong,
                                      leewaySeconds: CLongLong,
                                      leewayNanoseconds: CLongLong,
                                      clock: CInt,
                                      job unownedJob: UnownedJob) {
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
}
#endif
