## Concurrency Runtime

This is a general guide to help you get started understanding the Swift concurrency runtime.
For high level semantics refer to Swift Evolution proposals, this document is aimed at developers working in the concurrency runtime, and discusses features which are outside of the scope of Swift evolution, such as runtime optimizations and implementation techniques.

### Actors and Executors

Swift's concurrency model is largely based around actors (and tasks).

**Actors** are the primary way of ensuring "isolation". 

Actors execute at most one task at any given time. However a task may be suspended while "on" an actor. When this happens, another task may execute on the actor -- this is called "actor reentrancy.

***

### Executor Switching

Swift performs so called "switching" when executing a task and it has to "hop" between different executor (e.g. actors).
If Swift naively enqueued a job every single time we called some async function, the overhead of async functions or actors 
would be prohibitive, therefore Swift employs a technique called (actor) switching.

Switching is a runtime optimization, and has no effect on semantic isolation of a program.

For this discussion the two kinds of `Executor` sub-types that matter are:
- `SerialExecutor` which ensures isolation, i.e. it can be used to guarantee the isolation an actor provides - this is why it is "serial"
- `TaskExecutor` which may be used to ensure a task prefers to run on a specific task executor which may be using a specific thread or group of threads. Task executors do not provide isolation.

