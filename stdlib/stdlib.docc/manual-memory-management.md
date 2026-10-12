# Manual Memory Management

Allocate and manage memory manually.

## Topics

### First Steps

- <doc:calling-functions-with-pointer-parameters>

### Safe Memory Access

- ``Swift/Span``
- ``Swift/RawSpan``
- ``Swift/OutputSpan``
- ``Swift/OutputRawSpan``
- ``Swift/UTF8Span``
- ``Swift/MutableSpan``
- ``Swift/MutableRawSpan``
- ``Swift/SpanIterator``

### Safe Access to Raw Bytes

- ``Swift/FullyInhabited``
- ``Swift/ConvertibleFromBytes``
- ``Swift/ConvertibleToBytes``
- ``Swift/ByteOrder``
- ``Swift/bitCast(_:to:)``
### Typed Pointers

Use typed pointers and buffers to access memory as instances of a specific type.

- ``Swift/UnsafePointer``
- ``Swift/UnsafeMutablePointer``
- ``Swift/UnsafeBufferPointer``
- ``Swift/UnsafeMutableBufferPointer``

### Raw Pointers

Use raw pointers and buffers to access memory for loading and storing as raw bytes.

- ``Swift/UnsafeRawPointer``
- ``Swift/UnsafeMutableRawPointer``
- ``Swift/UnsafeRawBufferPointer``
- ``Swift/UnsafeMutableRawBufferPointer``

### Memory Access

- ``Swift/withUnsafePointer(to:_:)-9fjn6``
- ``Swift/withUnsafePointer(to:_:)-35wrn``
- ``Swift/withUnsafeMutablePointer(to:_:)``
- ``Swift/withUnsafeBytes(of:_:)-3ywhh``
- ``Swift/withUnsafeMutableBytes(of:_:)``
- ``Swift/withTemporaryAllocation(byteCount:alignment:_:)``
- ``Swift/withTemporaryAllocation(of:capacity:_:)``
- ``Swift/withUnsafeTemporaryAllocation(of:capacity:_:)``
- ``Swift/withUnsafeTemporaryAllocation(byteCount:alignment:_:)``
- ``Swift/swap(_:_:)``
- ``Swift/exchange(_:with:)``

### Memory Layout

- ``Swift/MemoryLayout``

### Heap Storage

- ``Swift/UniqueArray``
- ``Swift/UniqueBox``

### Reference Counting

- ``Swift/Unmanaged``
- ``Swift/withExtendedLifetime(_:_:)-4mmpv``
- ``Swift/withExtendedLifetime(_:_:)-59dz3``
- ``Swift/extendLifetime(_:)``
