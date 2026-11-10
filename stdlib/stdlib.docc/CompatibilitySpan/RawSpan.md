# ``/CompatibilitySpan/RawSpan``


## Topics

### Creating a raw span

- ``init()``
- ``init(bytesOf:)``
- ``init(elements:)``
- ``init(unsafeElements:)``

### Accessing span elements

- ``byteCount``
- ``subscript(_:)``
- ``subscript(unchecked:)``

### Working with bytes

- ``byteCount``
- ``byteOffsets``
- ``byteOffsets(of:)``

### Comparing span relationships

- ``isIdentical(to:)``
- ``isTriviallyIdentical(to:)``

### Performing unsafe access

- ``withUnsafeBytes(_:)``

### Extracting span contents

- ``extracting(_:)-(Range<Int>)``                      <!-- @_lifetime(copy self) @export(implementation) func extracting(_ bounds: Range<Int>) -> RawSpan -->
- ``extracting(_:)-(RangeExpression<Int>)``            <!-- @_lifetime(copy self) @export(implementation) func extracting(_ bounds: some RangeExpression<Int>) -> RawSpan -->
- ``extracting(_:)-(UnboundedRange)``                  <!-- @_lifetime(copy self) @export(implementation) func extracting(_: UnboundedRange) -> RawSpan -->
- ``extracting(unchecked:)-(Range<Int>)``              <!-- @_lifetime(copy self) @unsafe @export(implementation) func extracting(unchecked bounds: Range<Int>) -> RawSpan -->
- ``extracting(unchecked:)-(ClosedRange<Int>)``        <!-- @_lifetime(copy self) @unsafe @export(implementation) func extracting(unchecked bounds: ClosedRange<Int>) -> RawSpan -->
- ``extracting(first:)``
- ``extracting(last:)``
- ``extracting(droppingFirst:)``
- ``extracting(droppingLast:)``

### Loading contents into types

- ``load(fromByteOffset:as:)``
- ``load(fromByteOffset:as:_:)``
- ``unsafeLoad(fromByteOffset:as:)``
- ``unsafeLoad(fromUncheckedByteOffset:as:)``
- ``unsafeLoadUnaligned(fromByteOffset:as:)``
- ``unsafeLoadUnaligned(fromUncheckedByteOffset:as:)``

