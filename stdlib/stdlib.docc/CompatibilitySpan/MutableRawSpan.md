# ``/CompatibilitySpan/MutableRawSpan``


## Topics

### Creating a raw span

- ``init()``
- ``init(bytesOf:)``
- ``init(elements:)``
- ``init(unsafeElements:)``
- ``init(mutating:)``

### Accessing span elements

- ``byteCount``
- ``isEmpty``
- ``subscript(_:)``
- ``subscript(unchecked:)``

### Working with bytes

- ``bytes``
- ``byteCount``
- ``byteOffsets``
- ``byteOffsets(of:)``

### Performing unsafe access

- ``withUnsafeBytes(_:)``
- ``withUnsafeMutableBytes(_:)``

### Loading contents into types

- ``load(fromByteOffset:as:)``
- ``load(fromByteOffset:as:_:)``
- ``unsafeLoad(fromByteOffset:as:)``
- ``unsafeLoad(fromUncheckedByteOffset:as:)``
- ``unsafeLoadUnaligned(fromByteOffset:as:)``
- ``unsafeLoadUnaligned(fromUncheckedByteOffset:as:)``

### Storing types into spans

- ``storeBytes(of:toByteOffset:as:_:)``
- ``storeBytes(of:toByteOffset:as:)-1afju``            <!-- @_lifetime(self: copy self) @export(implementation) mutating func storeBytes<T>(of value: T, toByteOffset offset: Int, as type: T.Type) where T : BitwiseCopyable, T : ConvertibleToBytes -->
- ``storeBytes(of:toByteOffset:as:)-37pwo``            <!-- @_lifetime(self: copy self) @unsafe @export(implementation) mutating func storeBytes<T>(of value: T, toByteOffset offset: Int = 0, as type: T.Type) where T : BitwiseCopyable -->
- ``storeBytes(of:toUncheckedByteOffset:as:)``
- ``storeBytes(repeating:count:as:)-6822y``            <!-- @_lifetime(self: copy self) @unsafe @export(implementation) mutating func storeBytes<T>(repeating repeatedValue: T, count: Int, as type: T.Type) where T : BitwiseCopyable -->
- ``storeBytes(repeating:count:as:)-7cd7p``            <!-- @_lifetime(self: copy self) @export(implementation) mutating func storeBytes<T>(repeating repeatedValue: T, count: Int, as type: T.Type) where T : BitwiseCopyable, T : ConvertibleToBytes -->
- ``storeBytes(repeating:count:as:_:)``

### Deprecated

- ``extracting(_:)-(Range<Int>)``                      <!-- @_lifetime(&self) @export(implementation) mutating func extracting(_ bounds: Range<Int>) -> MutableRawSpan -->
- ``extracting(_:)-(RangeExpression<Int>)``            <!-- @_lifetime(&self) @export(implementation) mutating func extracting(_ bounds: some RangeExpression<Int>) -> MutableRawSpan -->
- ``extracting(_:)-(UnboundedRange)``                  <!-- @_lifetime(&self) @export(implementation) mutating func extracting(_: UnboundedRange) -> MutableRawSpan -->
- ``extracting(unchecked:)-(Range<Int>)``              <!-- @_lifetime(&self) @unsafe @export(implementation) mutating func extracting(unchecked bounds: Range<Int>) -> MutableRawSpan -->
- ``extracting(unchecked:)-(ClosedRange<Int>)``        <!-- @_lifetime(&self) @unsafe @export(implementation) mutating func extracting(unchecked bounds: ClosedRange<Int>) -> MutableRawSpan -->
- ``extracting(first:)``
- ``extracting(last:)``
- ``extracting(droppingFirst:)``
- ``extracting(droppingLast:)``
