# ``/CompatibilitySpan/MutableSpan``


## Topics

### Creating a Span

- ``init()``
- ``init(mutableBytes:)``
- ``init(mutating:)``
- ``init(ofOne:)``

### Accessing Span Elements

- ``count``
- ``isEmpty``
- ``indices``
- ``subscript(_:)``
- ``subscript(unchecked:)``
- ``Index``

### Accessing Bytes

- ``bytes-478ye``        <!-- @export(implementation) var bytes: RawSpan { borrowing get } -->
- ``bytes-61tq``         <!-- @export(implementation) @unsafe var bytes: RawSpan { borrowing get } -->
- ``mutableBytes-7cwoq`` <!-- @export(implementation) @unsafe var mutableBytes: MutableRawSpan { mutating get } -->
- ``mutableBytes-9ha97`` <!-- @export(implementation) var mutableBytes: MutableRawSpan { mutating get } -->
- ``span``

### Performing Unsafe Access

- ``withUnsafeBytes(_:)``
- ``withUnsafeMutableBytes(_:)``
- ``withUnsafeBufferPointer(_:)``
- ``withUnsafeMutableBufferPointer(_:)``

### Swapping Span Contents

- ``swapAt(_:_:)``
- ``swapAt(unchecked:unchecked:)``

### Performing Bulk Updates

- ``update(repeating:)``

### Deprecated

- ``extracting(_:)-(Range<Index>)``               <!-- @_lifetime(&self) @export(implementation) mutating func extracting(_ bounds: Range<Index>) -> MutableSpan<Element> -->
- ``extracting(_:)-(RangeExpression<Index>)``     <!-- @_lifetime(&self) @export(implementation) mutating func extracting(_ bounds: some RangeExpression<Index>) -> MutableSpan<Element> -->
- ``extracting(_:)-(UnboundedRange)``             <!-- @_lifetime(&self) @export(implementation) mutating func extracting(_: UnboundedRange) -> MutableSpan<Element> -->
- ``extracting(first:)``
- ``extracting(last:)``
- ``extracting(droppingFirst:)``
- ``extracting(droppingLast:)``
- ``extracting(unchecked:)-(Range<Index>)``       <!-- @_lifetime(&self) @unsafe @export(implementation) mutating func extracting(unchecked bounds: Range<Index>) -> MutableSpan<Element> -->
- ``extracting(unchecked:)-(ClosedRange<Index>)`` <!-- @_lifetime(&self) @unsafe @export(implementation) mutating func extracting(unchecked bounds: ClosedRange<Index>) -> MutableSpan<Element> -->

