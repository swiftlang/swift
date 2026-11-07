/*===--- EmbeddedPlatform+Concurrency.h - Embedded Swift executor --*- C -*-===*
 *
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2026 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https://swift.org/LICENSE.txt for license information
 * See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 *
 *===----------------------------------------------------------------------===*/

#ifndef EMBEDDED_SWIFT_PLATFORM_CONCURRENCY_H
#define EMBEDDED_SWIFT_PLATFORM_CONCURRENCY_H

#include "EmbeddedPlatform.h"
#include "ExecutorTypes.h"

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * Enqueues a job on the global executor.
 *
 * The executor takes ownership of `job`. It must run the job once, at some
 * later point, by passing it to `swift_job_runSynchronously` together with
 * `swift_executor_generic()`.
 */
void _swift_concurrency_enqueueGlobal(SwiftJob * EMBEDDED_SWIFT_NONNULL job);

/**
 * Runs jobs on the calling thread.
 *
 * An `async` main function calls this function after running its main task up
 * to the task's first suspension. The jobs it runs must include the ones
 * enqueued on the main executor. The main task ends the program by calling
 * `exit`.
 */
void _swift_concurrency_asyncMain(void) EMBEDDED_SWIFT_NORETURN;

/**
 * Runs jobs from the global executor on the calling thread until
 * `condition(conditionContext)` returns `true`, or until no job is left.
 *
 * Support for this function is optional. Only
 * `swift_task_donateThreadToGlobalExecutorUntil` calls it, and an executor
 * that does not support it can define it to trap.
 */
void _swift_concurrency_donateThreadToGlobalExecutorUntil(
    bool (* EMBEDDED_SWIFT_NONNULL condition)(void * EMBEDDED_SWIFT_NULLABLE),
    void * EMBEDDED_SWIFT_NULLABLE conditionContext);

#if defined(__cplusplus)
}
#endif

#endif
