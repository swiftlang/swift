# ``Swift/TaskGroup``

## Topics

### Adding Tasks to a Task Group

- ``Swift/TaskGroup/addTask(priority:operation:)``
- ``Swift/TaskGroup/addTask(name:priority:operation:)``
- ``Swift/TaskGroup/addTask(executorPreference:priority:operation:)``
- ``Swift/TaskGroup/addTask(name:executorPreference:priority:operation:)``
- ``Swift/TaskGroup/addTaskUnlessCancelled(name:executorPreference:priority:operation:)``
- ``Swift/TaskGroup/addTaskUnlessCancelled(executorPreference:priority:operation:)``
- ``Swift/TaskGroup/addTaskUnlessCancelled(name:priority:operation:)``
- ``Swift/TaskGroup/addTaskUnlessCancelled(priority:operation:)``
- ``Swift/TaskGroup/addImmediateTask(name:priority:executorPreference:operation:)``
- ``Swift/TaskGroup/addImmediateTaskUnlessCancelled(name:priority:executorPreference:operation:)``

### Accessing Individual Results

- ``Swift/TaskGroup/next()``
- ``Swift/TaskGroup/next(isolation:)``
- ``Swift/TaskGroup/isEmpty``
- ``Swift/TaskGroup/waitForAll(isolation:)``

### Accessing an Asynchronous Sequence of Results

- ``Swift/TaskGroup/makeAsyncIterator()``
- ``Swift/TaskGroup/allSatisfy(_:)``
- ``Swift/TaskGroup/compactMap(_:)-944od``
- ``Swift/TaskGroup/compactMap(_:)-7mgj1``
- ``Swift/TaskGroup/contains(_:)``
- ``Swift/TaskGroup/contains(where:)``
- ``Swift/TaskGroup/drop(while:)``
- ``Swift/TaskGroup/dropFirst(_:)``
- ``Swift/TaskGroup/filter(_:)``
- ``Swift/TaskGroup/first(where:)``
- ``Swift/TaskGroup/flatMap(_:)-vhi3``
- ``Swift/TaskGroup/map(_:)-58nsr``
- ``Swift/TaskGroup/map(_:)-4a4kq``
- ``Swift/TaskGroup/max()``
- ``Swift/TaskGroup/max(by:)``
- ``Swift/TaskGroup/min()``
- ``Swift/TaskGroup/min(by:)``
- ``Swift/TaskGroup/prefix(_:)``
- ``Swift/TaskGroup/prefix(while:)``
- ``Swift/TaskGroup/reduce(_:_:)``
- ``Swift/TaskGroup/reduce(into:_:)``

### Canceling Tasks

- ``Swift/TaskGroup/isCancelled``
- ``Swift/TaskGroup/cancelAll()``

### Supporting Types

- ``Swift/TaskGroup/Element``
- ``Swift/TaskGroup/Iterator``
- ``Swift/TaskGroup/AsyncIterator``

### Deprecated

- ``Swift/TaskGroup/add(priority:operation:)``
- ``Swift/TaskGroup/async(priority:operation:)``
- ``Swift/TaskGroup/asyncUnlessCancelled(priority:operation:)``
- ``Swift/TaskGroup/spawn(priority:operation:)``
- ``Swift/TaskGroup/spawnUnlessCancelled(priority:operation:)``
