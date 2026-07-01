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

// Concrete SIMD types

func simdConformancesA(
  _ a: SIMD2<UInt8>, _ b: SIMD4<UInt8>, _ c: SIMD8<UInt8>,
  _ d: SIMD16<UInt8>, _ e: SIMD32<UInt8>, _ f: SIMD64<UInt8>,
  _ g: SIMD2<UInt16>, _ h: SIMD4<UInt16>, _ i: SIMD8<UInt16>,
  _ j: SIMD16<UInt16>, _ k: SIMD32<UInt16>, _ l: SIMD64<UInt16>
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

func simdConformancesB(
  _ a: SIMD2<UInt32>, _ b: SIMD4<UInt32>, _ c: SIMD8<UInt32>,
  _ d: SIMD16<UInt32>, _ e: SIMD32<UInt32>, _ f: SIMD64<UInt32>,
  _ g: SIMD2<UInt64>, _ h: SIMD4<UInt64>, _ i: SIMD8<UInt64>,
  _ j: SIMD16<UInt64>, _ k: SIMD32<UInt64>, _ l: SIMD64<UInt64>
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

func simdConformancesC(
  _ a: SIMD2<Float32>, _ b: SIMD4<Float32>, _ c: SIMD8<Float32>,
  _ d: SIMD16<Float32>, _ e: SIMD32<Float32>, _ f: SIMD64<Float32>,
  _ g: SIMD2<Float64>, _ h: SIMD4<Float64>, _ i: SIMD8<Float64>,
  _ j: SIMD16<Float64>, _ k: SIMD32<Float64>, _ l: SIMD64<Float64>
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

func simd3Conformances(
  _ a: SIMD3<UInt8>, _ b: SIMD3<UInt16>, _ c: SIMD3<UInt32>,
  _ d: SIMD3<UInt64>, _ e: SIMD3<Float32>, _ f: SIMD3<Float64>
) {
  fromBytes(a)
  fromBytes(b)
  fromBytes(c)
  fromBytes(d)
  fromBytes(e)
  fromBytes(f)
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

func simd3NotToBytes(_ a: SIMD3<Int>) {
  toBytes(a) // expected-error {{global function 'toBytes' requires that 'SIMD3<Int>' conform to 'ConvertibleToBytes'}}
             // expected-note@-221 {{where 'T' = 'SIMD3<Int>'}}
}
