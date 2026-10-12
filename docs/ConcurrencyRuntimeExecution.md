# How the Concurrency Runtime Runs Tasks: Executors, Actors and Jobs

This document describes how the Swift concurrency runtime runs tasks. It covers
jobs and tasks, serial executors, task executors and task executor preferences,
the default executor and actors. Furthermore, it explains how the runtime
enqueues jobs, how it runs them, and how it decides whether a task runs inline,
switches or is enqueued when it hops to another executor.

The implementation lives in `stdlib/public/Concurrency`. The most important
files are
- `Actor.cpp` (running jobs, executor hops, default actors)
- `Task.cpp` (enqueuing tasks, continuations)
- `TaskPrivate.h`
- `ExecutorTracking.h` (the per-thread record)

## 1. Jobs and tasks

### Jobs

A *job* is a unit of work that the runtime gives to an executor. Every job has a
header. Among others, it has the following fields:

- `SchedulerPrivate`, two words that the executor can use for storage while the
  job is enqueued on it. An executor uses them, for example, to link the job
  into its queue.
- `Flags`, which hold the kind of job and its priority.
- The function that runs the job.

### Tasks

A *task* is a job that runs an asynchronous function in parts. Each time the
task's job runs, it runs the function from where the task last suspended until
it suspends again or finishes. A task suspends, for example, to wait for another
task, to wait for a continuation, or to hop to another executor. When the task
can continue, the runtime enqueues it again.

## 2. Executors

### Serial executors

A *serial executor* runs one job at a time. There are two kinds:

- The *default actor serial executor*. The runtime implements it itself, with a
  queue and a lock stored in the actor object.
- A *custom serial executor*, a type that conforms to `SerialExecutor`. It
  receives jobs through `enqueue` and runs them with one of the
  `ExecutorJob.runSynchronously` methods. The main executor is a custom serial
  executor.

The runtime compares serial executors by identity, which is the executor object.
Two references to the same object are the same serial executor.

A custom serial executor can additionally opt into *complex equality*. It does
so by creating its `UnownedSerialExecutor` with `init(complexEquality:)`. Two
such executors of the same type count as the same serial executor if
`isSameExclusiveExecutionContext(other:)` returns `true`, even if they are
different objects. An example is two executors whose dispatch queues target the
same serial queue. The runtime only uses complex equality to check isolation,
for example in `assumeIsolated`, `preconditionIsolated` and isolated `deinit`.
It never uses it for executor hops which compare based on identities only.

Code that isn't isolated uses no serial executor. This document uses *none* to
indicate that. The runtime represents it as the *generic executor*, a serial
executor reference without an identity.

### Task executors and task executor preferences

A *task executor*, a type that conforms to `TaskExecutor`, provides the threads
that tasks run on. A task can *prefer* a task executor:

- `Task(executorPreference:)` and `addTask(executorPreference:)` give the task
  an initial preference.
- `withTaskExecutorPreference` sets a preference for the duration of its
  operation and removes it afterwards.

### The default executor

The *default executor* is the task executor that runs tasks which prefer no task
executor. It is set up before the first job is enqueued and never changes
afterwards.

## 3. Actors and isolation

### Actors

Every actor has a serial executor. It is either a custom serial executor, which
the actor declares through `unownedExecutor`, or the default actor serial
executor. The runtime calls an actor that uses the default actor serial executor
a *default actor*. The main actor uses the main executor.

### Isolation of code

Every piece of code a task runs is either isolated to an actor or to the task
itself:

- Code isolated to an actor needs the actor's serial executor.
- *Concurrent code* is isolated to the task. Examples are `@concurrent`
  functions and closures and the body of `Task.detached`. Concurrent code runs
  on the task's preferred task executor, or of the default executor if the task
  prefers none.
- A `nonisolated(nonsending)` function runs with its caller's isolation.
- A function with an `isolated` parameter runs on the parameter's isolation.

## 4. The execution context

### The context a job runs in

An executor runs a job in an *execution context*. The executor passes the
context to the runtime when it runs the job. This document writes it as `(S,
T)`:

- `S` is the serial executor the thread runs as. It is a custom serial executor,
  such as an event loop or the main executor, or *none*.
- `T` is the task executor the thread belongs to. It is either a specific task
  executor, *default* for the default executor, or *none*. A specific task
  executor that is the default executor is treated as *default*.

### The requirements of a job

A task's next piece of code has requirements to its *isolation* and
*preference*. This document writes them as `r(I, P)`:

- `I` is the serial executor the code is isolated to, or *none* for concurrent
  code.
- `P` is the task's preferred task executor, or *default* if it prefers none. A
  preference for the default executor counts as no preference.

### Matching a preference

Concurrent code must run on the task's preference `P`. An execution context `(S,
T)` *matches* the preference `P` if:

- `T == P`. The execution context belongs to the preferred task executor.
- `S == P`. The execution context runs as an executor that is both a serial and
  a task executor, such as an event loop, and that executor is the preferred
  one. Such an executor receives the jobs of both roles through the same
  `enqueue`, so every thread it runs a job on is one of its threads in both
  roles. This covers contexts that pass only its serial role, for example a job
  it runs with `runSynchronously(on:)` and its serial executor, or an isolated
  `Task.immediate` started from one of its jobs.

All comparisons are by executor identity.

### The per-thread record

The runtime tracks the current execution context of each thread in an
`ExecutorTrackingInfo` record. A thread local variable points to the current
record. A record holds:

- The *base* execution context, which is the context the job was started in.
- The serial executor of the *current* execution context. The current context
  has the base's task executor.
- A pointer to the record that was current before it.

The runtime can change the current context's serial executor during a job. It
never changes the task executor or the base. Records nest. Creating a record
makes it the current one, and leaving it restores the previous one.

## 5. Enqueuing a job

A task is enqueued through `swift_task_enqueue(job, I)`, which gives the job to
the executor that runs code with the isolation `I`:

- If `I` is none and the task prefers a task executor, the job goes to that task
  executor.
- If `I` is none and the task prefers none, the job goes to the default
  executor.
- If `I` is a custom serial executor, the job goes to that executor. The task's
  preference is ignored, because an actor with a custom serial executor runs its
  code on that executor whatever the preference.
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

An executor that is both a serial and a task executor receives jobs for both
roles through the same `enqueue`. It should therefore run every job passing both
roles, itself as the serial executor and itself, or its pool, as the task
executor. If it passes only the task executor, the context's serial executor is
none, so code isolated to the executor doesn't run inline. If it passes only the
serial executor, the context is `(E, none)`, so tasks without a preference don't
run inline, even if `E` is the default executor.

### Starting a task

For a task, the runtime does the following in the job's record:

1. It marks the task as running.
2. It makes the task the thread's current task, which is another thread local
   variable.
3. It runs the task until it suspends or finishes.
4. It restores the previous current task.

Other jobs it runs directly.

## 7. Default actors

### States

A default actor's state is one of the following:

- *Idle*. No job is queued and no thread runs the actor.
- *Scheduled*. Jobs are queued and a processing job has been enqueued to run
  them.
- *Running*. A thread runs the actor.
- *Zombie, ready for deallocation*. The actor was released while running and is
  deallocated when it is unlocked.

### Enqueuing on a default actor

Enqueuing a job on a default actor links the job into the actor's list of
incoming jobs. If the actor was idle, it becomes scheduled, and the runtime
enqueues a *processing job* with the job's priority. A processing job is a job
of its own, not a task, without a task executor preference. The runtime
therefore enqueues it directly on the preferred task executor of the task whose
job scheduled the actor, or on the default executor if that task prefers none,
rather than through `swift_task_enqueue`.

### The processing job

A processing job does the following:

1. It locks the actor.
2. It creates a record that has the base context of the record it runs in. The
   current context is the actor's serial executor together with the base's task
   executor.
3. It sorts the incoming jobs into the actor's queue by priority and runs them
   one after the other. A task that runs can leave the actor. If the task moved
   to another default actor, the processing job continues with that actor's
   queue. If the task unlocked the actor to run concurrent code, the processing
   job stops.
4. When no job is left, it unlocks the actor, which becomes idle. If a job was
   enqueued in the meantime, unlocking fails and the processing job runs that
   job too.

A task can also leave the actor while other jobs are still queued, or a job can
return while the actor is still locked. The runtime then unlocks the actor, the
actor becomes scheduled again, and the runtime enqueues a new processing job. It
enqueues it on the default executor.

### Isolated deinit

An isolated `deinit` runs inline if the current context is isolated to the
`deinit`'s serial executor. The runtime decides this with the same check as
`assumeIsolated`, so complex equality applies. Moreover, on the main thread the
check counts the main executor as current even without a record.

Otherwise, if the object is a default actor and the runtime can lock it, the
`deinit` runs inline with the actor's serial executor. Otherwise the runtime
enqueues an *isolated deinit job* on the serial executor.

## 8. Executor hops

Compiled code hops whenever the isolation of the code changes, for example when
it calls into or returns from an actor, or when it resumes after an `await`.
Each hop calls `swift_task_switch`, which decides what happens with the task's
next piece of code `r(I, P)` in the current context `(S, T)`. One of three
things happens:

1. **Running inline.** The current context can run the code as it is, so the
   thread runs it. Nothing changes.
2. **Switching.** The current context can't run the code as it is, but the
   runtime changes the current context so that it can, and the thread keeps
   running the task without enqueuing it.
3. **Enqueuing.** The runtime enqueues the task on `I`, or for concurrent code
   on `P`, and the thread returns to the job it was running.

### Running inline

| Code to run | Runs inline when |
|---|---|
| Isolated to a custom serial executor `I` | `S == I` |
| Isolated to a default actor `I` | `S == I` |
| Concurrent | `S` is not a default actor, and `(S, T)` matches `P` |

Code isolated to an actor runs inline when the current context has the actor's
serial executor. An actor with a custom serial executor runs its code on that
executor whatever the task's preference, as documented on
`withTaskExecutorPreference`. A default actor runs on its tasks' preferred task
executors, because that is where its processing jobs are enqueued. However, a
context that already has the default actor's serial executor keeps running it,
because unlocking the actor only to enqueue the same task again would add a
round trip without changing where the code runs.

Concurrent code runs inline in a context that matches the task's preference.
Which serial executor the context has doesn't matter. Moreover, the current
context keeps its serial executor while the thread runs the concurrent code. A
call back into code isolated to `S` therefore runs inline too, and `S` counts as
the current executor. This is sound because the thread is busy running this
task, so none of `S`'s other jobs can run on it in the meantime.

A context with a default actor's serial executor never runs concurrent code
inline. The runtime unlocks the actor first by switching, so that the actor can
run its other jobs elsewhere.

`canRunConcurrentCodeInline` implements the rule for concurrent code. For code
isolated to an actor, `swift_task_switch` compares the identities directly.

### Switching

The runtime can only lock and unlock the default actor serial executor, because
it implements that executor itself. A custom serial executor of a context is up
to the executor that runs the job, and the runtime can't change it. However, the
runtime can lock a default actor on top of the base context and unlock it again,
which returns the current context to the base.

When the code can't run inline, the runtime switches only if both of the
following hold:

- The base context doesn't have a default actor's serial executor. The runtime
  only unlocks default actors that it locked on top of the base during the job.
- The task isn't in the synchronous start of `Task.immediate`, during which the
  caller needs its thread back.

Then the runtime switches in the following cases:

- Code isolated to a default actor `I` that is idle, if the base context matches
  the task's preference `P`. The runtime unlocks the default actor of the
  current context, if any, and locks `I`. The current context is then `(I, the
  base's task executor)`. A default actor that is running or scheduled can't be
  locked this way.
- Concurrent code, while the current context has a default actor's serial
  executor, if the base context matches `P`. The runtime unlocks the actor, and
  the current context is then the base context again, where the code runs inline
  by the rule above.

The base context must match `P` in both cases, because a default actor runs on
its tasks' preferred task executors, and concurrent code runs on a thread of
`P`. A default actor that the runtime locks on top of the base runs with the
base's task executor.

`continueTaskWithSerialExecutor` changes the current context's serial executor
in the job's record, rather than creating a new record, and continues running
the task. When the job returns, its entry point unlocks a default actor that the
runtime locked during the job and that is still the current context's serial
executor.

### Enqueuing

When the runtime neither runs the code inline nor switches, it clears the
thread's current task and calls `flagAsAndEnqueueOnExecutor(I)`. The task then
runs again when the executor runs its job.

## 9. Task.immediate

`Task.immediate` starts a task synchronously on its caller's thread, until the
task first suspends:

- If the operation isn't isolated, the runtime runs the task with
  `swift_job_run` and a serial executor of none that carries an *immediate*
  marker. The runtime doesn't switch while that marker is set, because the
  caller needs its thread back. A hop to a default actor is therefore enqueued.
- If the operation is isolated, the caller must already run with the operation's
  serial executor. The runtime runs the task with `swift_job_run` and that
  executor. The base context then has that executor, so the runtime doesn't
  unlock it, neither while the task runs nor when the job returns.

## 10. Continuations

A continuation suspends a task until other code resumes it. Resuming the
continuation makes the task runnable on its *resume executor*, which enqueues it
as section 5 describes. The resume executor is none, unless the creator of the
continuation gave one to `swift_continuation_init` itself. With none, the task
goes to its preferred task executor, or to the default executor if it prefers
none. Code after the `await` then hops to its own isolation.

If the continuation is resumed before it is awaited, the runtime only records
that it was resumed. The task then continues inline when it reaches the `await`.
The exception is a continuation whose creator gave the resume executor to
`swift_continuation_init` itself and asked for a forced switch. The task then
hops to the resume executor when it reaches the `await`.

## 11. The main executor

The main executor runs jobs with only its serial executor, so the context is
`(main, none)`. Main actor code runs inline. Concurrent code and calls to
default actors are enqueued, because the context doesn't match any preference.

## 12. Implementations of the default executor

The runtime provides the following implementations of the default executor:

- The Dispatch based one. It enqueues a job on Dispatch's global concurrent
  queue for the job's priority. Dispatch then runs the job through `jobInvoke`,
  which calls `swift_job_run(job, S)`. `S` is none for a job on a global queue,
  and the queue's own serial executor for a job on a serial dispatch queue. A
  job on a global queue therefore runs in the context `(none, default)`, because
  only tasks without a specific preference end up there. On Apple platforms the
  main executor is Dispatch's main queue.
- The cooperative one, which runs all jobs on a single thread with
  `swift_job_run` and a serial executor of none. Platforms without Dispatch use
  it.
- A custom one, installed through an `ExecutorFactory` together with a custom
  main executor. It runs jobs with `runSynchronously(on: UnownedTaskExecutor)`,
  so the context is `(none, default)`.
