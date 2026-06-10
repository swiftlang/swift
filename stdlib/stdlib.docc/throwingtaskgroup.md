# ``Swift/ThrowingTaskGroup``

## Topics

### Adding Tasks to a Throwing Task Group

- ``Swift/ThrowingTaskGroup/addTask(priority:operation:)``
- ``Swift/ThrowingTaskGroup/addTask(executorPreference:priority:operation:)``
- ``Swift/ThrowingTaskGroup/addTask(name:priority:operation:)``
- ``Swift/ThrowingTaskGroup/addTask(name:executorPreference:priority:operation:)``
- ``Swift/ThrowingTaskGroup/addTaskUnlessCancelled(priority:operation:)``
- ``Swift/ThrowingTaskGroup/addTaskUnlessCancelled(name:executorPreference:priority:operation:)``
- ``Swift/ThrowingTaskGroup/addTaskUnlessCancelled(executorPreference:priority:operation:)``
- ``Swift/ThrowingTaskGroup/addTaskUnlessCancelled(name:priority:operation:)``
- ``Swift/ThrowingTaskGroup/addImmediateTask(name:priority:executorPreference:operation:)``
- ``Swift/ThrowingTaskGroup/addImmediateTaskUnlessCancelled(name:priority:executorPreference:operation:)``

### Accessing Individual Results

- ``Swift/ThrowingTaskGroup/next()``
- ``Swift/ThrowingTaskGroup/nextResult(isolation:)``
- ``Swift/ThrowingTaskGroup/next(isolation:)``
- ``Swift/ThrowingTaskGroup/isEmpty``
- ``Swift/ThrowingTaskGroup/waitForAll(isolation:)``

### Accessing an Asynchronous Sequence of Results

- ``Swift/ThrowingTaskGroup/makeAsyncIterator()``
- ``Swift/ThrowingTaskGroup/allSatisfy(_:)``
- ``Swift/ThrowingTaskGroup/compactMap(_:)-944nh``
- ``Swift/ThrowingTaskGroup/compactMap(_:)-7mgi5``
- ``Swift/ThrowingTaskGroup/contains(_:)``
- ``Swift/ThrowingTaskGroup/contains(where:)``
- ``Swift/ThrowingTaskGroup/drop(while:)``
- ``Swift/ThrowingTaskGroup/dropFirst(_:)``
- ``Swift/ThrowingTaskGroup/filter(_:)``
- ``Swift/ThrowingTaskGroup/first(where:)``
- ``Swift/ThrowingTaskGroup/flatMap(_:)-dwsi``
- ``Swift/ThrowingTaskGroup/flatMap(_:)-319er``
- ``Swift/ThrowingTaskGroup/flatMap(_:)-8ved0``
- ``Swift/ThrowingTaskGroup/flatMap(_:)->AsyncThrowingFlatMapSequence<Self,SegmentOfResult>``
- ``Swift/ThrowingTaskGroup/map(_:)-58nrv``
- ``Swift/ThrowingTaskGroup/map(_:)-4a4ju``
- ``Swift/ThrowingTaskGroup/max()``
- ``Swift/ThrowingTaskGroup/max(by:)``
- ``Swift/ThrowingTaskGroup/min()``
- ``Swift/ThrowingTaskGroup/min(by:)``
- ``Swift/ThrowingTaskGroup/prefix(_:)``
- ``Swift/ThrowingTaskGroup/prefix(while:)``
- ``Swift/ThrowingTaskGroup/reduce(_:_:)``
- ``Swift/ThrowingTaskGroup/reduce(into:_:)``

### Canceling Tasks

- ``Swift/ThrowingTaskGroup/isCancelled``
- ``Swift/ThrowingTaskGroup/cancelAll()``

### Supporting Types

- ``Swift/ThrowingTaskGroup/Element``
- ``Swift/ThrowingTaskGroup/Iterator``
- ``Swift/ThrowingTaskGroup/AsyncIterator``

### Deprecated

- ``Swift/ThrowingTaskGroup/add(priority:operation:)``
- ``Swift/ThrowingTaskGroup/async(priority:operation:)``
- ``Swift/ThrowingTaskGroup/asyncUnlessCancelled(priority:operation:)``
- ``Swift/ThrowingTaskGroup/spawn(priority:operation:)``
- ``Swift/ThrowingTaskGroup/spawnUnlessCancelled(priority:operation:)``
