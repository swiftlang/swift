// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_pointwise_multiplicative_other_module.swift

func testPointwiseMultiplicative<T : PointwiseMultiplicative>(
  _ x: inout T
) {
  // Test `PointwiseMultiplicative` requirements: `one`, `*`.
  let one = T.one
  x *= x * one
}

struct Empty : PointwiseMultiplicative {}
func testEmpty() {
  var empty = Empty()
  testPointwiseMultiplicative(&empty)
}

struct Int2: PointwiseMultiplicative {
  var a: Int
  var b: Int
}
func testInt2() {
  var int2 = Int2(a: 1, b: 1)
  testPointwiseMultiplicative(&int2)
}

// Test generic type.
struct Vector2<T : PointwiseMultiplicative>: PointwiseMultiplicative {
  var x: T
  var y: T
}
func testVector2() {
  var vec2 = Vector2<Double>(x: 1, y: 1)
  testPointwiseMultiplicative(&vec2)
}

// Test nested type.
struct Nested: PointwiseMultiplicative {
  var int2: Int2
  var int: Int
}
func testNested(int2: Int2) {
  var nested = Nested(int2: int2, int: 1)
  testPointwiseMultiplicative(&nested)
}

// Test mixed type.
// Note: `Numeric` refines `PointwiseMultiplicative`.
struct Mixed: PointwiseMultiplicative {
  var nested: Nested
  var float: Float
  var uint8: UInt8
}
func testMixed(nested: Nested) {
  var mixed = Mixed(nested: nested, float: 1, uint8: 1)
  testPointwiseMultiplicative(&mixed)
}

// Test type in generic context.
struct A<T> {
  struct B<U, V> {
    struct GenericContextNested : PointwiseMultiplicative {
      var nested: Nested
      var float: Float
      var uint8: UInt8
    }
  }
}
func testGenericContext<T, U, V>(nested: Nested) -> A<T>.B<U, V>.GenericContextNested {
  var genericNested =
    A<T>.B<U, V>.GenericContextNested(nested: nested, float: 1, uint8: 1)
  testPointwiseMultiplicative(&genericNested)
  return genericNested
}

// Test extension.
struct Extended {
  var x: Int
}
extension Extended : Equatable, PointwiseMultiplicative {}

// Test extension of generic type.
struct GenericExtended<T> {
  var x: T
}
extension GenericExtended : Equatable, PointwiseMultiplicative where T : PointwiseMultiplicative {}

// Test memberwise initializer synthesis.
struct NoMemberwiseInitializer<T : PointwiseMultiplicative> : PointwiseMultiplicative {
  var value: T
  init(randomLabel value: T) { self.value = value }
}
struct NoMemberwiseInitializerCustomZero: PointwiseMultiplicative {
  var x: Float
  static var zero: Self { return NoMemberwiseInitializerCustomZero(0) }
  init(_ x: Float) {
    self.x = x
  }
} struct NoMemberwiseInitializerExtended<T> {
  var value: T
  init(_ value: T) {
    self.value = value
  }
}
extension NoMemberwiseInitializerExtended: Equatable, PointwiseMultiplicative
  where T : PointwiseMultiplicative {}

// Test derived conformances in disallowed contexts.

// expected-error @+1 3 {{implementation of 'PointwiseMultiplicative' cannot be automatically synthesized in an extension in a different file to the type}}
extension OtherFileNonconforming : PointwiseMultiplicative {}

// expected-error @+1 3 {{implementation of 'PointwiseMultiplicative' cannot be automatically synthesized in an extension in a different file to the type}}
extension GenericOtherFileNonconforming : PointwiseMultiplicative {}
