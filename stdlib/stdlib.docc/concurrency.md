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
- 
### Task-Local Storage

- ``Swift/TaskLocal``
- ``Swift/TaskLocal()``

### Task Deadlines

- ``Swift/withDeadline(_:tolerance:clock:operation:)``
- ``Swift/withDeadline(in:tolerance:clock:operation:)``
- ``Swift/Task/hasActiveDeadline``
- ``Swift/Task/activeDeadline(for:)``

### Task Cancellation

- ``Swift/Task/isCancelled-type.property``
- ``Swift/Task/cancel()``
- ``Swift/Task/cancel(reason:)``
- ``Swift/Task/cancellationReason``
- ``Swift/Task/checkCancellation()``
- ``Swift/CancellationError``
- ``Swift/CancellationError/Reason``
- ``Swift/withTaskCancellationHandler(operation:onCancel:)``
- ``Swift/withTaskCancellationHandler(operation:onCancel:isolation:)``
- ``Swift/withTaskCancellationShield(operation:)-8zlgh``
- ``Swift/withTaskCancellationShield(operation:)-2lzl8``
- ``Swift/Task/hasActiveCancellationShield``

### Asynchronous Sequences

- ``Swift/AsyncSequence``
- ``Swift/AsyncStream``
- ``Swift/AsyncThrowingStream``

### Continuations

- ``Swift/Continuation``
- ``Swift/withContinuation(of:_:)``
- ``Swift/withContinuation(of:throwing:_:)``
- ``Swift/CheckedContinuation``
- ``Swift/withCheckedContinuation(function:_:)``
- ``Swift/withCheckedThrowingContinuation(function:_:)-13yf6``
- ``Swift/withCheckedThrowingContinuation(function:_:)-2k46m``
- ``Swift/UnsafeContinuation``
- ``Swift/withUnsafeContinuation(_:)``
- ``Swift/withUnsafeThrowingContinuation(_:)-32nwt``
- ``Swift/withUnsafeThrowingContinuation(_:)-7zhvy``

### Actors

- ``Swift/Sendable``
- ``Swift/Actor``
- ``Swift/MainActor``
- ``Swift/GlobalActor``
- ``Swift/SendableMetatype``
- ``Swift/isolation()-u1o6``

### Executors

- ``Swift/Executor``
- ``Swift/ExecutorJob``
- ``Swift/SerialExecutor``
- ``Swift/TaskExecutor``
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

- ``Swift/extractIsolation(_:)``
- ``Swift/withCheckedContinuation(isolation:function:_:)``
- ``Swift/withCheckedThrowingContinuation(isolation:function:_:)``
- ``Swift/withUnsafeContinuation(isolation:_:)``
- ``Swift/AnyActor``
- ``Swift/ConcurrentValue``
- ``Swift/Job``
- ``Swift/PartialAsyncTask``
- ``Swift/UnsafeConcurrentValue``
- ``Swift/UnsafeSendable``
- ``Swift/UnsafeThrowingContinuation``
- ``Swift/withUnsafeThrowingContinuation(isolation:_:)``
- ``Swift/withUnsafeThrowingContinuation(isolation:_:)``
