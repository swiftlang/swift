# ``/CompatibilitySpan/Span``

## Topics

### Creating a Span

- ``init()``
- ``init(ofOne:)``
- ``init(viewing:)``

### Accessing span elements

- ``count``
- ``isEmpty``
- ``indices``
- ``subscript(_:)``
- ``subscript(unchecked:)``
- ``Index``

### Comparing span relationships

- ``isIdentical(to:)``
- ``isTriviallyIdentical(to:)``
- ``indices(of:)``

### Accessing bytes

- ``bytes-6qp42`` <!-- @export(implementation) @unsafe var bytes: RawSpan { get } -->
- ``bytes-8rxg``  <!-- @export(implementation) var bytes: RawSpan { get } -->

### Performing unsafe access

- ``withUnsafeBufferPointer(_:)``
- ``withUnsafeBytes(_:)``

### Extracting span contents

- ``extracting(_:)-(Range<Index>)``               <!-- @_lifetime(copy self) @export(implementation) func extracting(_ bounds: Range<Index>) -> Span<Element> -->
- ``extracting(_:)-(RangeExpression<Index>)``     <!-- @_lifetime(copy self) @export(implementation) func extracting(_ bounds: some RangeExpression<Index>) -> Span<Element> -->
- ``extracting(_:)-(UnboundedRange)``             <!-- @_lifetime(copy self) @export(implementation) func extracting(_: UnboundedRange) -> Span<Element> -->
- ``extracting(unchecked:)-(ClosedRange<Index>)`` <!-- @_lifetime(copy self) @unsafe @export(implementation) func extracting(unchecked bounds: ClosedRange<Index>) -> Span<Element> -->
- ``extracting(unchecked:)-(Range<Index>)``       <!-- @_lifetime(copy self) @unsafe @export(implementation) func extracting(unchecked bounds: Range<Index>) -> Span<Element> -->
- ``extracting(first:)``
- ``extracting(last:)``
- ``extracting(droppingFirst:)``
- ``extracting(droppingLast:)``

