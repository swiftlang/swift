// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated \
// RUN:     -disable-availability-checking

func toBytes<T: ConvertibleToBytes>(_: T) {}
func fromBytes<T: ConvertibleFromBytes>(_: T) {} // expected-note 4 {{where 'T'}}
func fullyInhabited<T: ConvertibleToBytes & ConvertibleFromBytes>(_: T) {}

// Integer types

func integerConformances(
  _ a: UInt8, _ b: Int8,
  _ c: UInt16, _ d: Int16,
  _ e: UInt32, _ f: Int32,
  _ g: UInt64, _ h: Int64,
  _ i: UInt, _ j: Int,
  _ k: UInt128, _ l: Int128
) {
  fullyInhabited(a)
  fullyInhabited(b)
  fullyInhabited(c)
  fullyInhabited(d)
  fullyInhabited(e)
  fullyInhabited(f)
  fullyInhabited(g)
  fullyInhabited(h)
  fullyInhabited(i)
  fullyInhabited(j)
  fullyInhabited(k)
  fullyInhabited(l)
}

// Floating-point types

func floatingPointConformances(
  _ f32: Float32, _ f64: Float64
) {
  fullyInhabited(f32)
  fullyInhabited(f64)
}

// Duration

func durationConformances(_ a: Duration) {
  fullyInhabited(a)
}

// InlineArray

func inlineArrayConformances(
  _ a: InlineArray<3, UInt8>, _ b: InlineArray<2, Int64>
) {
  fullyInhabited(a)
  fullyInhabited(b)
}

// CollectionOfOne

func collectionOfOneConformances(
  _ a: CollectionOfOne<UInt32>, _ b: CollectionOfOne<Float64>
) {
  fullyInhabited(a)
  fullyInhabited(b)
}

// Range types (ConvertibleToBytes only)

func rangeConformances(
  _ a: Range<Int>, _ b: ClosedRange<Int>
) {
  toBytes(a)
  toBytes(b)
}

// Partial range types

func partialRangeConformances(
  _ a: PartialRangeFrom<Int>,
  _ b: PartialRangeThrough<Int>,
  _ c: PartialRangeUpTo<Int>
) {
  fullyInhabited(a)
  fullyInhabited(b)
  fullyInhabited(c)
}

func partialRangeFromIteratorConformances(
  _ a: PartialRangeFrom<Int>.Iterator
) {
  fullyInhabited(a)
}

// Bool

func boolConformances(_ a: Bool) {
  toBytes(a)
}

// ObjectIdentifier

func objectIdentifierConformances(_ a: ObjectIdentifier) {
  toBytes(a)
}

// Pointer types

func pointerConformances(
  _ a: UnsafePointer<Int>,
  _ b: UnsafeMutablePointer<Int>,
  _ c: UnsafeRawPointer,
  _ d: UnsafeMutableRawPointer,
  _ e: OpaquePointer
) {
  toBytes(a)
  toBytes(b)
  toBytes(c)
  toBytes(d)
  toBytes(e)
}

// Buffer pointer types

func bufferPointerConformances(
  _ a: UnsafeBufferPointer<Int>,
  _ b: UnsafeMutableBufferPointer<Int>,
  _ c: UnsafeRawBufferPointer,
  _ d: UnsafeMutableRawBufferPointer
) {
  toBytes(a)
  toBytes(b)
  toBytes(c)
  toBytes(d)
}

// Non-conformances

func boolNotFromBytes(_ a: Bool) {
  fromBytes(a) // expected-error {{global function 'fromBytes' requires that 'Bool' conform to 'ConvertibleFromBytes'}}
}

func rangeNotFromBytes(_ a: Range<Int>, _ b: ClosedRange<Int>) {
  fromBytes(a) // expected-error {{global function 'fromBytes' requires that 'Range<Int>' conform to 'ConvertibleFromBytes'}}
  fromBytes(b) // expected-error {{global function 'fromBytes' requires that 'ClosedRange<Int>' conform to 'ConvertibleFromBytes'}}
}

func objectIdentifierNotFromBytes(_ a: ObjectIdentifier) {
  fromBytes(a) // expected-error {{global function 'fromBytes' requires that 'ObjectIdentifier' conform to 'ConvertibleFromBytes'}}
}
