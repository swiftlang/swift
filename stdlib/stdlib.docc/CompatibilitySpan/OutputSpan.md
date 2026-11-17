# ``/CompatibilitySpan/OutputSpan``


## Topics

### Creating a Span

- ``init()``
- ``init(buffer:initializedCount:)-(UnsafeMutableBufferPointer<Element>,_)``        <!-- @_lifetime(borrow buffer) @unsafe @export(implementation) init(buffer: UnsafeMutableBufferPointer<Element>, initializedCount: Int) -->
- ``init(buffer:initializedCount:)-(Slice<UnsafeMutableBufferPointer<Element>>,_)`` <!-- @_lifetime(borrow buffer) @unsafe @export(implementation) init(buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>, initializedCount: Int) -->

### Accessing Span Elements

- ``count``
- ``capacity``
- ``freeCapacity``
- ``isEmpty``
- ``isFull``
- ``indices``
- ``subscript(_:)``
- ``subscript(unchecked:)``
- ``Index``

### Appending Span Contents

- ``append(_:)``
- ``append(repeating:count:)``

### Swapping Span Contents

- ``swapAt(_:_:)``
- ``swapAt(unchecked:unchecked:)``

### Removing Span Contents

- ``removeAll()``
- ``removeLast()``
- ``removeLast(_:)``

### Accessing Initialized Memory

- ``Span``
- ``mutableSpan``

### Retrieving Initialized Memory

- ``finalize(for:)-(Slice<UnsafeMutableBufferPointer<Element>>)`` <!-- @unsafe @export(implementation) consuming func finalize(for buffer: Slice<UnsafeMutableBufferPointer<Element>>) -> Int -->
- ``finalize(for:)-(UnsafeMutableBufferPointer<Element>)``        <!-- @unsafe @export(implementation) consuming func finalize(for buffer: UnsafeMutableBufferPointer<Element>) -> Int -->

### Performing Unsafe Access

- ``withUnsafeMutableBufferPointer(_:)``

