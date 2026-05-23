# ``Swift/Int``

## Topics

### Converting Integers

- ``Swift/Int/init(_:)-4ekvl``
- ``Swift/Int/init(exactly:)-b1dy``
- ``Swift/Int/init(clamping:)``
- ``Swift/Int/init(truncatingIfNeeded:)``
- ``Swift/Int/init(bitPattern:)-72037``

### Converting Floating-Point Values

- ``Swift/Int/init(_:)-6gt9z``
- ``Swift/Int/init(_:)-8vbwo``
- ``Swift/Int/init(_:)-2oscb``
- ``Swift/Int/init(_:)-3huv0``
- ``Swift/Int/init(_:)-66i0w``

### Converting with No Loss of Precision

These initializers result in `nil` if the value passed can’t be represented without
any loss of precision.

- ``Swift/Int/init(exactly:)-7yhn6``
- ``Swift/Int/init(exactly:)-77kq8``
- ``Swift/Int/init(exactly:)-7qdwf``
- ``Swift/Int/init(exactly:)-5xh2s``
- ``Swift/Int/init(exactly:)-5kot1``

### Converting Strings

- ``Swift/Int/init(_:)-2hmii``
- ``Swift/Int/init(_:radix:)``

### Creating a Random Integer

- ``Swift/Int/random(in:)-9mjpw``
- ``Swift/Int/random(in:using:)-4lsb5``
- ``Swift/Int/random(in:)-8zzqh``
- ``Swift/Int/random(in:using:)-3dwv4``

### Performing Calculations

- <doc:integer-operators>
- ``Swift/Int/negate()``
- ``Swift/Int/quotientAndRemainder(dividingBy:)``
- ``Swift/Int/isMultiple(of:)``

### Performing Calculations with Overflow

These methods return the result of an operation, and a flag indicating whether the
operation overflowed the bounds of the type.

- ``Swift/Int/addingReportingOverflow(_:)``
- ``Swift/Int/subtractingReportingOverflow(_:)``
- ``Swift/Int/multipliedReportingOverflow(by:)``
- ``Swift/Int/dividedReportingOverflow(by:)``
- ``Swift/Int/remainderReportingOverflow(dividingBy:)``

### Performing Double-Width Calculations

- ``Swift/Int/multipliedFullWidth(by:)``
- ``Swift/Int/dividingFullWidth(_:)``

### Finding the Sign and Magnitude

- ``Swift/Int/magnitude-swift.property``
- ``Swift/Int/Magnitude-swift.typealias``
- ``Swift/abs(_:)``
- ``Swift/Int/signum()``

### Accessing Numeric Constants

- ``Swift/Int/zero``
- ``Swift/Int/min``
- ``Swift/Int/max``
- ``Swift/Int/isSigned``

### Working with Byte Order

- ``Swift/Int/byteSwapped``
- ``Swift/Int/littleEndian``
- ``Swift/Int/bigEndian``
- ``Swift/Int/init(littleEndian:)``
- ``Swift/Int/init(bigEndian:)``

### Working with Binary Representation

- ``Swift/Int/bitWidth-swift.type.property``
- ``Swift/Int/bitWidth-swift.property``
- ``Swift/Int/nonzeroBitCount``
- ``Swift/Int/leadingZeroBitCount``
- ``Swift/Int/trailingZeroBitCount``
- ``Swift/Int/words-swift.property``
- ``Swift/Int/Words-swift.struct``

### Working with Memory Addresses

These initializers create an integer with the bit pattern of the memory address of
a pointer or class instance.

- ``Swift/Int/init(bitPattern:)-2i0qy``
- ``Swift/Int/init(bitPattern:)-2o9co``
- ``Swift/Int/init(bitPattern:)-5qm7a``

### Encoding and Decoding Values

- ``Swift/Int/encode(to:)``
- ``Swift/Int/init(from:)-5ru5``

### Describing an Integer

- ``Swift/Int/description``
- ``Swift/Int/hash(into:)``
- ``Swift/Int/customMirror``

### Infrequently Used Functionality

- ``Swift/Int/init()``
- ``Swift/Int/init(integerLiteral:)``
- ``Swift/Int/IntegerLiteralType``
- ``Swift/Int/distance(to:)``
- ``Swift/Int/advanced(by:)``

### Deprecated

- ``Swift/Int/customPlaygroundQuickLook``

### SIMD-Supporting Types

- ``Swift/Int/SIMDMaskScalar``
- ``Swift/Int/SIMD2Storage``
- ``Swift/Int/SIMD4Storage``
- ``Swift/Int/SIMD8Storage``
- ``Swift/Int/SIMD16Storage``
- ``Swift/Int/SIMD32Storage``
- ``Swift/Int/SIMD64Storage``
