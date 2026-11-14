# ``/CompatibilitySpan/RawSpan``


## Topics

### Creating a Raw Span

- ``init()``
- ``init(bytesOf:)``
- ``init(elements:)``
- ``init(unsafeElements:)``

### Accessing Span Elements

- ``byteCount``
- ``isEmpty``
- ``subscript(_:)``
- ``subscript(unchecked:)``

### Working with Bytes

- ``byteCount``
- ``byteOffsets``
- ``byteOffsets(of:)``

### Comparing Span Relationships

- ``isIdentical(to:)``
- ``isTriviallyIdentical(to:)``

### Performing Unsafe Access

- ``withUnsafeBytes(_:)``

### Extracting Span Contents

- ``extracting(_:)-(Range<Int>)``                      <!-- @_lifetime(copy self) @export(implementation) func extracting(_ bounds: Range<Int>) -> RawSpan -->
- ``extracting(_:)-(RangeExpression<Int>)``            <!-- @_lifetime(copy self) @export(implementation) func extracting(_ bounds: some RangeExpression<Int>) -> RawSpan -->
- ``extracting(_:)-(UnboundedRange)``                  <!-- @_lifetime(copy self) @export(implementation) func extracting(_: UnboundedRange) -> RawSpan -->
- ``extracting(unchecked:)-(Range<Int>)``              <!-- @_lifetime(copy self) @unsafe @export(implementation) func extracting(unchecked bounds: Range<Int>) -> RawSpan -->
- ``extracting(unchecked:)-(ClosedRange<Int>)``        <!-- @_lifetime(copy self) @unsafe @export(implementation) func extracting(unchecked bounds: ClosedRange<Int>) -> RawSpan -->
- ``extracting(first:)``
- ``extracting(last:)``
- ``extracting(droppingFirst:)``
- ``extracting(droppingLast:)``

### Loading Contents into Types

- ``load(fromByteOffset:as:)``
- ``load(fromByteOffset:as:_:)``
- ``unsafeLoad(fromByteOffset:as:)``
- ``unsafeLoad(fromUncheckedByteOffset:as:)``
- ``unsafeLoadUnaligned(fromByteOffset:as:)``
- ``unsafeLoadUnaligned(fromUncheckedByteOffset:as:)``

