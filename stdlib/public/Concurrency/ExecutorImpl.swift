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
  #if !$Embedded
  if #available(StdlibDeploymentTarget 6.3, *) {
    if let runnableExecutor = Task.defaultExecutor as? RunLoopExecutor {
      try! runnableExecutor.runUntil { unsafe Bool(condition(context)) }
    } else {
      fatalError("Global executor does not support thread donation")
    }
  } else {
    fatalError("this should never happen")
  }
  #else
  fatalError("Global executor does not support thread donation")
  #endif
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

#if !$Embedded && !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
/// The default executor, which has to be able to schedule work against the
/// given clock if these functions are going to work.
///
/// These functions are the runtime's fallback for scheduling a timer when
/// there is no scheduling executor to hand it to (`Task.sleep()` uses
/// them in that case), and they can only work if the default executor is a
/// `SchedulingExecutor` that says that it schedules against the clock.  We
/// check that it does, rather than asking it anyway, because an executor that
/// has said that it doesn't might hand the work to the clock, which would hand
/// it back to the runtime's global enqueue function, which would get us here
/// again, for ever.
///
/// If the default executor is not like that, an executor that wants to be used
/// in this way has to provide its own implementation of the `Impl` function, or
/// install a hook.
@available(StdlibDeploymentTarget 6.5, *)
private func defaultSchedulingExecutor(
  for clock: SystemClockID
) -> any SchedulingExecutor {
  guard let executor = Task.defaultExecutor.asSchedulingExecutor,
        executor.supportsScheduling(on: clock) else {
    fatalError("""
      the default executor does not support scheduling work for later on the \
      \(clock) clock; it must be a SchedulingExecutor that returns true from \
      supportsScheduling(on:) for it, or provide its own \
      swift_task_enqueueGlobalWithDelayImpl and \
      swift_task_enqueueGlobalWithDeadlineImpl
      """)
  }
  return executor
}
#endif

#if !$Embedded
@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalWithDelayImpl")
@diagnose(UselessAvailabilityCheck, as: ignored)
internal func enqueueOnGlobalExecutor(delay: CUnsignedLongLong,
                                      job unownedJob: UnownedJob) {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  if #available(StdlibDeploymentTarget 6.5, *) {
    _ = defaultSchedulingExecutor(for: .continuous).schedule(
      ExecutorJob(unownedJob),
      at: .after(.nanoseconds(delay)),
      clock: .continuous)
  } else {
    fatalError("this should never happen")
  }
  #else
  fatalError("swift_task_enqueueGlobalWithDelay() not supported for task-to-thread")
  #endif
}

@available(SwiftStdlib 6.2, *)
@_silgen_name("swift_task_enqueueGlobalWithDeadlineImpl")
@diagnose(UselessAvailabilityCheck, as: ignored)
internal func enqueueOnGlobalExecutor(seconds: CLongLong,
                                      nanoseconds: CLongLong,
                                      leewaySeconds: CLongLong,
                                      leewayNanoseconds: CLongLong,
                                      clock: CInt,
                                      job unownedJob: UnownedJob) {
  #if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  // The deadline is an absolute time on `clock`, so schedule it as one.
  let deadline = Duration(_seconds: seconds, nanoseconds: nanoseconds)

  // A negative tolerance means that none was specified.
  let leeway: Duration? = leewayNanoseconds < 0 ? nil
    : Duration.seconds(leewaySeconds) + Duration.nanoseconds(leewayNanoseconds)

  if #available(StdlibDeploymentTarget 6.5, *) {
    switch clock {
      case _ClockID.suspending.rawValue:
        _ = defaultSchedulingExecutor(for: .suspending).schedule(
          ExecutorJob(unownedJob),
          at: .at(SuspendingClock.Instant(_value: deadline)),
          tolerance: leeway,
          clock: .suspending
        )
      case _ClockID.continuous.rawValue:
        _ = defaultSchedulingExecutor(for: .continuous).schedule(
          ExecutorJob(unownedJob),
          at: .at(ContinuousClock.Instant(_value: deadline)),
          tolerance: leeway,
          clock: .continuous
        )
      default:
        fatalError("Unknown clock ID \(clock)")
    }
  } else {
    fatalError("this should never happen")
  }
  #else
  fatalError("swift_task_enqueueGlobalWithDeadline() not supported for task-to-thread")
  #endif
}
#endif
