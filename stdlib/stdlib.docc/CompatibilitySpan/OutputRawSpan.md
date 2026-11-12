# ``/CompatibilitySpan/OutputRawSpan``

<!-- The content below this line is auto-generated and is redundant. You should either incorporate it into your content above this line or delete it. -->

## Topics

### Creating a Span

- ``init()``
- ``init(buffer:initializedCount:)-(UnsafeMutableRawBufferPointer,_)``        <!-- @_lifetime(borrow buffer) @unsafe @export(implementation) init(buffer: UnsafeMutableRawBufferPointer, initializedCount: Int) -->
- ``init(buffer:initializedCount:)-(Slice<UnsafeMutableRawBufferPointer>,_)`` <!-- @_lifetime(borrow buffer) @unsafe @export(implementation) init(buffer: borrowing Slice<UnsafeMutableRawBufferPointer>, initializedCount: Int) -->

### Accessing Span Elements

- ``byteCount``
- ``bytes``
- ``capacity``
- ``freeCapacity``
- ``isEmpty``
- ``isFull``
- ``subscript(_:)``
- ``subscript(unchecked:)``

### Working with Bytes

- ``bytes``
- ``mutableBytes``
- ``byteCount``
- ``byteOffsets``

### Appending Span Contents

- ``append(_:)``
- ``append(_:as:)-63w17``                                   <!-- @_lifetime(self: copy self) @export(implementation) @unsafe mutating func append<T>(_ value: T, as type: T.Type) where T : BitwiseCopyable -->
- ``append(_:as:)-89j87``                                   <!-- @_lifetime(self: copy self) @export(implementation) mutating func append<T>(_ value: T, as type: T.Type) where T : BitwiseCopyable, T : ConvertibleToBytes -->
- ``append(_:as:_:)``
- ``append(repeating:count:as:)-1h8m1``                     <!-- @_lifetime(self: copy self) @export(implementation) @unsafe mutating func append<T>(repeating repeatedValue: T, count: Int, as type: T.Type) where T : BitwiseCopyable -->
- ``append(repeating:count:as:)-3z0bf``                     <!-- @_lifetime(self: copy self) @export(implementation) mutating func append<T>(repeating repeatedValue: T, count: Int, as type: T.Type) where T : BitwiseCopyable, T : ConvertibleToBytes -->
- ``append(repeating:count:as:_:)``

### Removing Span Contents

- ``removeAll()``
- ``removeLast()``
- ``removeLast(_:)``

### Retrieving Initialized Memory

- ``finalize(for:)-(Slice<UnsafeMutableRawBufferPointer>)`` <!-- @unsafe @export(implementation) consuming func finalize(for buffer: Slice<UnsafeMutableRawBufferPointer>) -> Int -->
- ``finalize(for:)-(UnsafeMutableRawBufferPointer)``        <!-- @unsafe @export(implementation) consuming func finalize(for buffer: UnsafeMutableRawBufferPointer) -> Int -->

### Performing Unsafe Access

- ``withUnsafeMutableBytes(_:)``

