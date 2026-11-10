# ``/CompatibilitySpan/OutputSpan``


## Topics

### Creating a span

- ``init()``
- ``init(buffer:initializedCount:)-(UnsafeMutableBufferPointer<Element>,_)``        <!-- @_lifetime(borrow buffer) @unsafe @export(implementation) init(buffer: UnsafeMutableBufferPointer<Element>, initializedCount: Int) -->
- ``init(buffer:initializedCount:)-(Slice<UnsafeMutableBufferPointer<Element>>,_)`` <!-- @_lifetime(borrow buffer) @unsafe @export(implementation) init(buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>, initializedCount: Int) -->

### Accessing span elements

- ``count``
- ``capacity``
- ``freeCapacity``
- ``isEmpty``
- ``isFull``
- ``indices``
- ``subscript(_:)``
- ``subscript(unchecked:)``
- ``Index``

### Appending span contents

- ``append(_:)``
- ``append(repeating:count:)``

### Swapping span contents

- ``swapAt(_:_:)``
- ``swapAt(unchecked:unchecked:)``

### Removing span contents

- ``removeAll()``
- ``removeLast()``
- ``removeLast(_:)``

### Accessing initialized memory

- ``span``
- ``mutableSpan``

### Retrieving initialized memory

- ``finalize(for:)-(Slice<UnsafeMutableBufferPointer<Element>>)`` <!-- @unsafe @export(implementation) consuming func finalize(for buffer: Slice<UnsafeMutableBufferPointer<Element>>) -> Int -->
- ``finalize(for:)-(UnsafeMutableBufferPointer<Element>)``        <!-- @unsafe @export(implementation) consuming func finalize(for buffer: UnsafeMutableBufferPointer<Element>) -> Int -->

### Performing unsafe access

- ``withUnsafeMutableBufferPointer(_:)``

