// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify %s -verify-ignore-unknown

func testVectorNumeric<T : VectorNumeric>(
_ x: inout T, scalar: T.Scalar
) {
  // Test `AdditiveArithmetic` requirements: `zero`, `+`, `-`.
  let zero = T.zero
  x += x + zero
  x -= x - zero
  // Test `VectorNumeric` requirements: `Scalar`, `*`.
  x *= scalar
  _ = scalar * x
  _ = x * scalar
}

struct Float2: VectorNumeric {
  var x: Float
  var y: Float
}
var float2 = Float2(x: 1, y: 1)
testVectorNumeric(&float2, scalar: 1)

// Test generic type.
struct Vector2<T : VectorNumeric>: VectorNumeric {
  var x: T
  var y: T
}
_ = Vector2<Double>(x: 1, y: 1)
_ = Vector2<Float2>(x: float2, y: float2)
func testGeneric<T : VectorNumeric>(vec2: inout Vector2<T>, scalar: T.Scalar) {
  testVectorNumeric(&vec2, scalar: scalar)
}

// Test nested types.
struct Nested: VectorNumeric {
  var float2: Float2
  var float: Float
}
var nested = Nested(float2: float2, float: 1)
testVectorNumeric(&nested, scalar: 1)

struct NestedGeneric: VectorNumeric {
  var vec2: Vector2<Float>
  var float2: Float2
  var float: Float
}
var nestedGeneric = NestedGeneric(vec2: Vector2<Float>(x: 1, y: 1),
                                  float2: float2, float: 1)
testVectorNumeric(&nestedGeneric, scalar: 1)

// Test type in generic context.
struct A<T> {
  struct B<U, V> {
    struct GenericContextNested : VectorNumeric {
      var float2: Float2
      var float: Float
    }
  }
}
func testGenericContext<T, U, V>() -> A<T>.B<U, V>.GenericContextNested {
  var genericNested =
    A<T>.B<U, V>.GenericContextNested(float2: float2, float: 1)
  testVectorNumeric(&genericNested, scalar: 1)
  return genericNested
}

// Test extension.
struct Extended {
  var x: Float
}
extension Extended : Equatable, AdditiveArithmetic, VectorNumeric {}

// Test extension of generic type.
struct GenericExtended<T> {
  var x: T
}
extension GenericExtended : Equatable, AdditiveArithmetic, VectorNumeric where T : VectorNumeric {}

// Test errors.

struct Empty : VectorNumeric {} // expected-error {{type 'Empty' does not conform to protocol 'VectorNumeric'}}

// Test type whose members conform to `VectorNumeric`
// but have different `Scalar` associated type.
struct InvalidMixedScalar: VectorNumeric { // expected-error {{type 'InvalidMixedScalar' does not conform to protocol 'VectorNumeric'}}
  var float: Float
  var double: Double
}

// Test initializer that is not a memberwise initializer because of stored property name vs parameter label mismatch.
struct HasCustomNonMemberwiseInitializer<T : VectorNumeric>: VectorNumeric {
  var value: T
  init(randomLabel value: T) { self.value = value }
}
