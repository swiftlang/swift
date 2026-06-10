# Concurrency

Perform asynchronous and parallel operations.

## Topics


### Tasks

- ``Swift/Task``
- ``Swift/TaskGroup``
- ``Swift/withTaskGroup(of:returning:isolation:body:)``
- ``Swift/ThrowingTaskGroup``
- ``Swift/withThrowingTaskGroup(of:returning:isolation:body:)``
- ``Swift/TaskPriority``
- ``Swift/DiscardingTaskGroup``
- ``Swift/withDiscardingTaskGroup(returning:isolation:body:)``
- ``Swift/ThrowingDiscardingTaskGroup``
- ``Swift/withThrowingDiscardingTaskGroup(returning:isolation:body:)``
- ``Swift/UnsafeCurrentTask``

### Asynchronous Sequences

- ``Swift/AsyncSequence``
- ``Swift/AsyncStream``
- ``Swift/AsyncThrowingStream``

### Continuations

- ``Swift/CheckedContinuation``
- ``Swift/withCheckedContinuation(isolation:function:_:)``
- ``Swift/withCheckedThrowingContinuation(isolation:function:_:)``
- ``Swift/UnsafeContinuation``
- ``Swift/withUnsafeContinuation(isolation:_:)``
- ``Swift/withUnsafeThrowingContinuation(isolation:_:)``

### Actors

- ``Swift/Sendable``
- ``Swift/Actor``
- ``Swift/AnyActor``
- ``Swift/MainActor``
- ``Swift/GlobalActor``
- ``Swift/SendableMetatype``
- ``Swift/ConcurrentValue``
- ``Swift/UnsafeSendable``
- ``Swift/UnsafeConcurrentValue``
- ``Swift/isolation()-u1o6``
- ``Swift/extractIsolation(_:)``

### Task-Local Storage

- ``Swift/TaskLocal``
- ``Swift/TaskLocal()``

### Executors

- ``Swift/Executor``
- ``Swift/ExecutorJob``
- ``Swift/SerialExecutor``
- ``Swift/TaskExecutor``
- ``Swift/PartialAsyncTask``
- ``Swift/UnownedJob``
- ``Swift/JobPriority``
- ``Swift/UnownedSerialExecutor``
- ``Swift/UnownedTaskExecutor``
- ``Swift/UnimplementedMainExecutor``
- ``Swift/UnimplementedTaskExecutor``
- ``SchedulingExecutor``
- ``Swift/globalConcurrentExecutor``
- ``Swift/withTaskExecutorPreference(_:isolation:operation:)``

### Main and Task Executors

- ``Swift/MainExecutor``
- ``Swift/RunLoopExecutor``
- ``Swift/ExecutorFactory``
- ``Swift/PlatformExecutorFactory``


### Deprecated

- ``Swift/Job``
- ``Swift/UnsafeThrowingContinuation``
