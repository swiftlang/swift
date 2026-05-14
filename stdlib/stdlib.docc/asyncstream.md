# ``Swift/AsyncStream``

## Topics

### Creating a Continuation-Based Stream

- ``Swift/AsyncStream/init(_:bufferingPolicy:_:)``
- ``Swift/AsyncStream/Continuation/BufferingPolicy``
- ``Swift/AsyncStream/Continuation``

### Creating a Stream from an Asynchronous Function

- ``Swift/AsyncStream/init(unfolding:onCancel:)``

### Finding Elements

- ``Swift/AsyncStream/contains(_:)``
- ``Swift/AsyncStream/contains(where:)``
- ``Swift/AsyncStream/allSatisfy(_:)``
- ``Swift/AsyncStream/first(where:)``
- ``Swift/AsyncStream/min()``
- ``Swift/AsyncStream/min(by:)``
- ``Swift/AsyncStream/max()``
- ``Swift/AsyncStream/max(by:)``

### Selecting Elements

- ``Swift/AsyncStream/prefix(_:)``
- ``Swift/AsyncStream/prefix(while:)``

### Excluding Elements

- ``Swift/AsyncStream/dropFirst(_:)``
- ``Swift/AsyncStream/drop(while:)``
- ``Swift/AsyncStream/filter(_:)``

### Transforming a Sequence

- ``Swift/AsyncStream/map(_:)-58nsf``
- ``Swift/AsyncStream/map(_:)-4a4la``
- ``Swift/AsyncStream/compactMap(_:)-7mgjd``
- ``Swift/AsyncStream/compactMap(_:)-944op``
- ``Swift/AsyncStream/flatMap(_:)-vhhr``
- ``Swift/AsyncStream/reduce(_:_:)``
- ``Swift/AsyncStream/reduce(into:_:)``

### Creating an Iterator

- ``Swift/AsyncStream/makeAsyncIterator()``
- ``Swift/AsyncStream/Iterator``

