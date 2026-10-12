# How the Concurrency Runtime Runs Tasks

This document describes how the Swift concurrency runtime runs tasks. It covers
jobs and tasks, serial executors, task executors and task executor preferences,
the default executor and actors. It also explains how the runtime enqueues jobs,
how it runs them, and how it decides if a task runs inline, switches or is
enqueued when it hops to another executor.

The implementation lives in `stdlib/public/Concurrency`. The most important
files are:
- `Actor.cpp` (running jobs, executor hops, default actors)
- `Task.cpp` (enqueuing tasks, continuations)
- `TaskPrivate.h`
- `ExecutorTracking.h` (the per-thread record)

## 1. Jobs and tasks

### Jobs

A *job* is a unit of work that the runtime gives to an executor. Every job has a
header. Among others the header has these fields:

- `SchedulerPrivate`, two words that the executor can use as storage while the
  job is enqueued on it. For example an executor can use them to link the job
  into its queue.
- `Flags`, which hold the kind of job and its priority.
- The function that runs the job.

### Tasks

A *task* is a job that runs an asynchronous function in parts. Each time the
task's job runs, it runs the function from where the task last suspended until
it suspends again or finishes. A task suspends for example to wait for another
task, to wait for a continuation or to hop to another executor. When the task
can continue, the runtime enqueues it again.

## 2. Executors

### Serial executors

A *serial executor* runs one job at a time. There are two kinds:

- The *default actor serial executor*. The runtime implements it itself with a
  queue and a lock that are stored in the actor object.
- A *custom serial executor*, a type that conforms to `SerialExecutor`. It
  receives jobs through `enqueue` and runs them with one of the
  `ExecutorJob.runSynchronously` methods. The main executor is a custom serial
  executor.

The runtime compares serial executors by identity. The identity is the executor
object, so two references to the same object are the same serial executor.

A custom serial executor can also opt into *complex equality* by creating its
`UnownedSerialExecutor` with `init(complexEquality:)`. Two such executors of the
same type count as the same serial executor if
`isSameExclusiveExecutionContext(other:)` returns `true`, even if they are
different objects. An example are two executors whose dispatch queues target the
same serial queue. The runtime only uses complex equality to check isolation,
for example in `assumeIsolated`, `preconditionIsolated` and isolated `deinit`.
Executor hops never use it, they only compare identities.

Code that isn't isolated uses no serial executor. This document writes *none*
for that. The runtime represents it as the *generic executor*, which is a serial
executor reference without an identity.

### Task executors and task executor preferences

A *task executor*, a type that conforms to `TaskExecutor`, provides the threads
that tasks run on. A task can *prefer* a task executor:

- `Task(executorPreference:)` and `addTask(executorPreference:)` give the task
  an initial preference.
- `withTaskExecutorPreference` sets a preference while its operation runs and
  removes it afterwards.

### The default executor

The *default executor* is the task executor that runs tasks that don't prefer a
task executor. It is set up before the first job is enqueued and doesn't change
afterwards.

## 3. Actors and isolation

### Actors

Every actor has a serial executor. It is either a custom serial executor that
the actor declares through `unownedExecutor`, or the default actor serial
executor. An actor that uses the default actor serial executor is called a
*default actor*. The main actor uses the main executor.

### Isolation of code

Every piece of code that a task runs is either isolated to an actor or to the
task itself:

- Code isolated to an actor needs the actor's serial executor.
- *Concurrent code* is isolated to the task. Examples are `@concurrent`
  functions and closures and the body of `Task.detached`. Concurrent code runs
  on the task's preferred task executor, or on the default executor if the task
  prefers none.
- A `nonisolated(nonsending)` function runs with its caller's isolation.
- A function with an `isolated` parameter runs with the parameter's isolation.

## 4. The execution context

### The context a job runs in

An executor runs a job in an *execution context*. The executor passes the
context to the runtime when it runs the job. This document writes it as `(S,
T)`:

- `S` is the serial executor the thread runs as. It is a custom serial executor,
  such as an event loop or the main executor, or *none*.
- `T` is the task executor the thread belongs to. It is either a specific task
  executor, *default* for the default executor, or *none*. If a specific task
  executor is the default executor, it is treated as *default*.

### The requirements of a job

The next piece of code of a task has two requirements, its *isolation* and its
*preference*. This document writes them as `r(I, P)`:

- `I` is the serial executor the code is isolated to, or *none* for concurrent
  code.
- `P` is the task's preferred task executor, or *default* if it prefers none. A
  preference for the default executor counts as no preference.

### Matching a preference

Concurrent code must run on the task's preference `P`. An execution context `(S,
T)` *matches* the preference `P` if one of these is true:

- `T == P`. The context belongs to the preferred task executor.
- `S == P`. The context runs as an executor that is both a serial and a task
  executor, for example an event loop, and this executor is the preferred one.
  Such an executor gets the jobs of both roles through the same `enqueue`. So
  every thread it runs a job on is one of its threads in both roles. This also
  covers contexts that only pass the serial role, for example a job that it runs
  with `runSynchronously(on:)` and its serial executor, or an isolated
  `Task.immediate` started from one of its jobs.

All comparisons use the executor identity.

### The per-thread record

The runtime tracks the current execution context of each thread in an
`ExecutorTrackingInfo` record. A thread local variable points to the current
record. A record has:

- The *base* execution context, which is the context the job was started in.
- The serial executor of the *current* execution context. The current context
  has the base's task executor.
- A pointer to the record that was current before it.

The runtime can change the serial executor of the current context during a job.
It never changes the task executor or the base. Records nest. Creating a record
makes it the current one and leaving it restores the previous one.

## 5. Enqueuing a job

A task is enqueued through `swift_task_enqueue(job, I)`, which gives the job to
the executor that runs code with the isolation `I`:

- If `I` is none and the task prefers a task executor, the job goes to that task
  executor.
- If `I` is none and the task prefers none, the job goes to the default
  executor.
- If `I` is a custom serial executor, the job goes to that executor. The task's
  preference is ignored, because an actor with a custom serial executor always
  runs its code on that executor.
- If `I` is a default actor serial executor, the job goes into the actor's
  queue.

`AsyncTask::flagAsAndEnqueueOnExecutor` records in the task's status that the
task is enqueued on `I`, and then gives the task to `swift_task_enqueue`.

## 6. Running a job

### Entry points

An executor runs a job with one of the following entry points.

| Entry point | Execution context |
|---|---|
| `swift_job_run(job, S)`, `ExecutorJob.runSynchronously(on: UnownedSerialExecutor)` | `(none, P)` if `S` is none, where `P` is the preference of the job's task, `(S, none)` otherwise |
| `swift_job_run_on_task_executor(job, T)`, `runSynchronously(on: UnownedTaskExecutor)` | `(none, T)` |
| `swift_job_run_on_serial_and_task_executor(job, S, T)`, `runSynchronously(isolatedTo:taskExecutor:)` | `(S, T)` |

All of them end up in `swift_job_run_on_serial_and_task_executor`, which does
the following:

1. It creates a record whose current and base context is the job's execution
   context.
2. It runs the job.
3. It restores the record that was current before.

An executor that is both a serial and a task executor gets the jobs for both
roles through the same `enqueue`. So it should run every job with both roles,
itself as the serial executor and itself or its pool as the task executor. If it
only passes the task executor, the serial executor of the context is none and
code isolated to the executor doesn't run inline. If it only passes the serial
executor, the context is `(E, none)` and concurrent code of tasks without a
preference doesn't run inline, even if `E` is the default executor.

### Starting a task

For a task, the runtime does the following in the job's record:

1. It marks the task as running.
2. It makes the task the current task of the thread. This is another thread
   local variable.
3. It runs the task until it suspends or finishes.
4. It restores the previous current task.

Other jobs are run directly.

## 7. Default actors

### States

A default actor's state is one of the following:

- *Idle*. No job is queued and no thread runs the actor.
- *Scheduled*. Jobs are queued and a processing job has been enqueued to run
  them.
- *Running*. A thread runs the actor.
- *Zombie, ready for deallocation*. The actor was released while it was running
  and is deallocated when it is unlocked.

### Enqueuing on a default actor

Enqueuing a job on a default actor links the job into the actor's list of
incoming jobs. If the actor was idle, it becomes scheduled and the runtime
enqueues a *processing job* with the job's priority. A processing job is its own
job and not a task, so it has no task executor preference. Because of that the
runtime doesn't use `swift_task_enqueue` for it. It enqueues it directly on the
preferred task executor of the task whose job scheduled the actor, or on the
default executor if that task prefers none.

### The processing job

A processing job does the following:

1. It locks the actor.
2. It creates a record with the base context of the record it runs in. The
   current context is the actor's serial executor together with the base's task
   executor.
3. It sorts the incoming jobs into the actor's queue by priority and runs them
   one after the other.
4. After its first job, it only runs a job if the job's task prefers the task
   executor that the processing job was enqueued on. No preference and a
   preference for the default executor both count as the default executor. Jobs
   that aren't tasks can run in any processing job. If the next job's task
   prefers another task executor, the processing job unlocks the actor and
   stops.
5. A task that runs can leave the actor. If the task moved to another default
   actor, the processing job continues with the queue of that actor. If the task
   unlocked the actor to run concurrent code, the processing job stops.
6. When no job is left, it unlocks the actor and the actor becomes idle. If a
   job was enqueued in the meantime, unlocking fails and the processing job runs
   that job too.

The processing job always runs its first job, whatever the job's task prefers.
This makes sure that every processing job makes progress. Usually the first job
matches the task executor the processing job was enqueued on. It doesn't match
if a job with a higher priority and another preference was enqueued before the
processing job started. In that case the first job runs on a thread of the wrong
task executor.

A task can also leave the actor while other jobs are still queued, a job can
return while the actor is still locked, or a processing job can stop because of
a job whose task prefers another task executor. In all these cases the runtime
unlocks the actor, the actor becomes scheduled again and the runtime enqueues a
new processing job. It enqueues it on the preferred task executor of the next
job's task, or on the default executor if that task prefers none.

### Isolated deinit

An isolated `deinit` runs inline if the current context is isolated to the
serial executor of the `deinit`. The runtime uses the same check as
`assumeIsolated` for this, so complex equality applies. On the main thread the
check also counts the main executor as current, even without a record.

If that isn't the case and the object is a default actor that the runtime can
lock, the `deinit` runs inline. It runs in a record that has the actor's serial
executor on top of the current context, like a processing job. Otherwise the
runtime enqueues an *isolated deinit job* on the serial executor.

## 8. Executor hops

Compiled code hops when the isolation of the code changes, for example when it
calls into or returns from an actor, or when it resumes after an `await`. Each
hop calls `swift_task_switch`. It decides what happens with the next piece of
code of the task `r(I, P)` in the current context `(S, T)`. There are three
outcomes:

1. **Running inline.** The current context can run the code, so the thread runs
   it. Nothing changes.
2. **Switching.** The current context can't run the code, but the runtime
   changes the current context so that it can. The thread keeps running the task
   without enqueuing it.
3. **Enqueuing.** The runtime enqueues the task on `I`, or on `P` for concurrent
   code, and the thread returns to the job it was running.

### Running inline

| Code to run | Runs inline when |
|---|---|
| Isolated to a custom serial executor `I` | `S == I` |
| Isolated to a default actor `I` | `S == I` |
| Concurrent | `S` is not a default actor, and `(S, T)` matches `P` |

Code isolated to an actor runs inline when the current context has the actor's
serial executor. An actor with a custom serial executor runs its code on that
executor whatever the task prefers, as documented on
`withTaskExecutorPreference`. A default actor runs on the preferred task
executors of its tasks, because that is where its processing jobs are enqueued.
But a context that already has the default actor's serial executor keeps running
it. Unlocking the actor only to enqueue the same task again would add a round
trip and wouldn't change where the code runs.

Concurrent code runs inline in a context that matches the task's preference. It
doesn't matter which serial executor the context has. The current context also
keeps its serial executor while the thread runs the concurrent code. Because of
that, a call back into code isolated to `S` runs inline too and `S` counts as
the current executor. This is sound because the thread is busy running this
task, so no other job of `S` can run on it in the meantime. The downside is that
`S` can't run its other jobs until the task suspends or leaves `S`, even though
the code doesn't need `S`.

A context with a default actor's serial executor never runs concurrent code
inline. The runtime first unlocks the actor by switching, so that the actor can
run its other jobs somewhere else.

`canRunConcurrentCodeInline` implements the rule for concurrent code. For code
isolated to an actor, `swift_task_switch` compares the identities directly.

### Switching

The runtime can only lock and unlock the default actor serial executor, because
it implements that executor itself. The custom serial executor of a context is
up to the executor that runs the job and the runtime can't change it. What the
runtime can do is lock a default actor on top of the base context and unlock it
again, which returns the current context to the base.

When the code can't run inline, the runtime only switches if both of these are
true:

- The base context doesn't have a default actor's serial executor. The runtime
  only unlocks default actors that it locked on top of the base during the job.
- The task isn't in the synchronous start of `Task.immediate`, during which the
  caller needs its thread back.

Then the runtime switches in these cases:

- Code isolated to the base's serial executor, while the current context has a
  default actor's serial executor. The runtime unlocks the actor, and the
  current context is then the base context again, where the code runs inline.
- Code isolated to a default actor `I` that is idle, if the base context matches
  the task's preference `P`. The runtime unlocks the default actor of the
  current context, if any, and locks `I`. The current context is then `(I, the
  base's task executor)`. A default actor that is running or scheduled can't be
  locked this way.
- Concurrent code, while the current context has a default actor's serial
  executor, if the base context matches `P`. The runtime unlocks the actor, and
  the current context is then the base context again, where the code runs inline
  by the rule above.

The base context must match `P` in the last two cases. A default actor runs on
the preferred task executors of its tasks and concurrent code runs on a thread
of `P`. A default actor that the runtime locks on top of the base runs with the
base's task executor.

`continueTaskWithSerialExecutor` changes the serial executor of the current
context in the job's record and continues running the task. It doesn't create a
new record. When the job returns, its entry point unlocks a default actor that
the runtime locked during the job, if it is still the serial executor of the
current context.

### Enqueuing

When the runtime doesn't run the code inline and doesn't switch, it clears the
current task of the thread and calls `flagAsAndEnqueueOnExecutor(I)`. The task
runs again when the executor runs its job.

## 9. Task.immediate

`Task.immediate` starts a task synchronously on its caller's thread, until the
task first suspends:

- If the operation isn't isolated, the runtime runs the task with
  `swift_job_run` and a serial executor of none that has an *immediate* marker.
  The runtime doesn't switch while the marker is set, because the caller needs
  its thread back. So a hop to a default actor is enqueued.
- If the operation is isolated, the caller must already run with the serial
  executor of the operation. The runtime runs the task with `swift_job_run` and
  that executor. The base context then has that executor, so the runtime doesn't
  unlock it, not while the task runs and not when the job returns.

Potentially unexpected: In the first case the context is `(none, P)`, like for
any job run with `swift_job_run` and no serial executor, although the thread is
the caller's thread and not a thread of `P`. Concurrent code of the task
therefore runs inline on the caller's thread until the task first suspends.

## 10. Continuations

A continuation suspends a task until other code resumes it. Resuming the
continuation enqueues the task on its *resume executor*, as described in section
5. The resume executor is none, unless the code that created the continuation
passed one to `swift_continuation_init`. If it is none, the task goes to its
preferred task executor, or to the default executor if it prefers none. The code
after the `await` then hops to its own isolation.

If the continuation is resumed before it is awaited, the runtime only records
that it was resumed. The task then continues inline when it reaches the `await`.
The exception is a continuation where the creator passed a resume executor to
`swift_continuation_init` and asked for a forced switch. In that case the task
hops to the resume executor when it reaches the `await`.

## 11. The main executor

The main executor runs jobs only with its serial executor, so the context is
`(main, none)`. Main actor code runs inline. Concurrent code and calls to
default actors are enqueued, because the context doesn't match any preference.

## 12. Implementations of the default executor

The runtime provides the following implementations of the default executor:

- The Dispatch based one. It enqueues a job on the Dispatch global concurrent
  queue for the job's priority. Dispatch then runs the job through `jobInvoke`,
  which calls `swift_job_run(job, S)`. `S` is none for a job on a global queue
  and the queue's serial executor for a job on a serial dispatch queue. So a job
  on a global queue runs in the context `(none, default)`, because only tasks
  without a specific preference end up there. On Apple platforms the main
  executor is the Dispatch main queue.
- The cooperative one. It runs all jobs on a single thread with `swift_job_run`
  and a serial executor of none. Platforms without Dispatch use it.
- A custom one, installed through an `ExecutorFactory` together with a custom
  main executor. It runs jobs with `runSynchronously(on: UnownedTaskExecutor)`,
  so the context is `(none, default)`.
