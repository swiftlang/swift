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

// None of TaskExecutor APIs are available in task-to-thread concurrency model.
#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY

/// The global concurrent executor that is used by default for Swift Concurrency
/// tasks.
///
/// The executor's implementation is platform dependent.
/// By default it uses a fixed size pool of threads and should not be used for
/// blocking operations which do not guarantee forward progress as doing so may
/// prevent other tasks from being executed and render the system unresponsive.
///
/// You may pass this executor explicitly to a ``Task`` initializer as a task
/// executor preference, in order to ensure and document that task be executed
/// on the global executor, instead e.g. inheriting the enclosing actor's
/// executor. Refer to ``withTaskExecutorPreference(_:operation:)`` for a
/// detailed discussion of task executor preferences.
///
/// Customizing the global concurrent executor is currently not supported.
@available(SwiftStdlib 6.0, *)
@_unavailableInEmbedded
public var globalConcurrentExecutor: any TaskExecutor {
  get {
    if #available(StdlibDeploymentTarget 6.2, *) {
      return Task.defaultExecutor
    } else {
      fatalError("we shouldn't get here; if we have, availability is broken")
    }
  }
}

/// A task executor which enqueues all work on the default global concurrent
/// thread pool that is used as the default executor for Swift concurrency
/// tasks.
@available(SwiftStdlib 6.0, *)
@_unavailableInEmbedded
internal final class _DefaultGlobalConcurrentExecutor: TaskExecutor {
  public static let shared: _DefaultGlobalConcurrentExecutor = .init()

  private init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    _enqueueJobGlobal(UnownedJob(job)._context)
  }

  public func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    // The "default global concurrent executor" is simply the "undefined" one.
    // We represent it as the `(0, 0)` ExecutorRef and it is handled properly
    // by the runtime, without having to call through to the
    // `_DefaultGlobalConcurrentExecutor` declared in Swift.
    unsafe UnownedTaskExecutor(_getUndefinedTaskExecutor())
  }
}

#endif
