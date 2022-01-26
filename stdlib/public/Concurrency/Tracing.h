//===--- Tracing.h - Support code for concurrency tracing ----------*- C++ -*-//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Support code for tracing events in the concurrency runtime
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_TRACING_H
#define SWIFT_CONCURRENCY_TRACING_H

#include <stdint.h>

namespace swift {
class AsyncLet;
class AsyncTask;
class ExecutorRef;
struct HeapObject;
class Job;
class TaskGroup;
class TaskStatusRecord;

namespace concurrency {
namespace trace {

// Actor trace calls.

void actor_create(HeapObject *actor);

void actor_destroy(HeapObject *actor);

void actor_deallocate(HeapObject *actor);

void actor_enqueue(HeapObject *actor, Job *job);

void actor_dequeue(HeapObject *actor, Job *job);

// The `flags` parameter is the raw values of the actor's
// DefaultActorImpl::State::Flags.
void actor_state_changed(HeapObject *actor, Job *firstJob,
                         bool needsPreprocessing, uintptr_t flags);

void actor_note_job_queue(HeapObject *actor, Job *first,
                          Job *(*getNext)(Job *));

// Task trace calls.

void task_create(AsyncTask *task, AsyncTask *parent, TaskGroup *group,
                 AsyncLet *asyncLet);

void task_destroy(AsyncTask *task);

// The `flags` parameter is the raw value of the ActiveTaskStatus::Flags field
// in the task.
void task_status_changed(AsyncTask *task, uintptr_t flags);

// The `flags` parameter is the raw value of Job::Flags.
void task_flags_changed(AsyncTask *task, uint32_t flags);

// The `status` parameter is the value of the corresponding
// FutureFragment::Status.
void task_wait(AsyncTask *task, AsyncTask *waitingOn, uintptr_t status);

void job_enqueue_global(Job *job);

void job_enqueue_global_with_delay(unsigned long long delay, Job *job);

void job_enqueue_main_executor(Job *job);

// This returns a handle that must be passed to the corresponding call to
// task_run_end.
uint64_t job_run_begin(Job *job, ExecutorRef *executor);

void job_run_end(Job *job, ExecutorRef *executor, uint64_t beginHandle);

} // namespace trace
} // namespace concurrency
} // namespace swift

#if __has_include(<os/signpost.h>)
#include "TracingSignpost.h"
#else
#include "TracingStubs.h"
#endif

#endif
