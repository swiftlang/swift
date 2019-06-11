// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_vector_protocol_other_module.swift

func testVectorProtocol<T : VectorProtocol>(
_ x: inout T, scalar: T.Scalar
) {
  // Test `AdditiveArithmetic` requirements: `zero`, `+`, `-`.
  let zero = T.zero
  x += x + zero
  x -= x - zero
  // Test `VectorProtocol` requirements: `Scalar`, `*`.
  x *= scalar
  _ = scalar * x
  _ = x * scalar
}

struct Float2: VectorProtocol {
  var x: Float
  var y: Float
}
func testFloat2() {
  var float2 = Float2(x: 1, y: 1)
  testVectorProtocol(&float2, scalar: 1)
}

// Test generic type.
struct Vector2<T : VectorProtocol>: VectorProtocol {
  var x: T
  var y: T
}
func testVector2(float2: Float2) {
  _ = Vector2<Double>(x: 1, y: 1)
  _ = Vector2<Float2>(x: float2, y: float2)
}
func testGeneric<T : VectorProtocol>(vec2: inout Vector2<T>, scalar: T.Scalar) {
  testVectorProtocol(&vec2, scalar: scalar)
}

// Test nested types.
struct Nested: VectorProtocol {
  var float2: Float2
  var float: Float
}
func testNested(float2: Float2) {
  var nested = Nested(float2: float2, float: 1)
  testVectorProtocol(&nested, scalar: 1)
}

struct NestedGeneric: VectorProtocol {
  var vec2: Vector2<Float>
  var float2: Float2
  var float: Float
}
func testNestedGeneric(float2: Float2) {
  var nestedGeneric = NestedGeneric(vec2: Vector2<Float>(x: 1, y: 1),
                                    float2: float2, float: 1)
  testVectorProtocol(&nestedGeneric, scalar: 1)
}

// Test type in generic context.
struct A<T> {
  struct B<U, V> {
    struct GenericContextNested : VectorProtocol {
      var float2: Float2
      var float: Float
    }
  }
}
func testGenericContext<T, U, V>(float2: Float2) -> A<T>.B<U, V>.GenericContextNested {
  var genericNested =
    A<T>.B<U, V>.GenericContextNested(float2: float2, float: 1)
  testVectorProtocol(&genericNested, scalar: 1)
  return genericNested
}

// Test extension.
struct Extended {
  var x: Float
}
extension Extended : Equatable, AdditiveArithmetic, VectorProtocol {}

// Test extension of generic type.
struct GenericExtended<T> {
  var x: T
}
extension GenericExtended : Equatable, AdditiveArithmetic, VectorProtocol where T : VectorProtocol {}

// Test errors.

struct Empty : VectorProtocol {} // expected-error {{type 'Empty' does not conform to protocol 'VectorProtocol'}}

// Test type whose members conform to `VectorProtocol`
// but have different `Scalar` associated type.
struct InvalidMixedScalar: VectorProtocol { // expected-error {{type 'InvalidMixedScalar' does not conform to protocol 'VectorProtocol'}}
  var float: Float
  var double: Double
}

// Test initializer that is not a memberwise initializer because of stored property name vs parameter label mismatch.
struct HasCustomNonMemberwiseInitializer<T : VectorProtocol>: VectorProtocol {
  var value: T
  init(randomLabel value: T) { self.value = value }
}

// Test derived conformances in disallowed contexts.

// expected-error @+2 {{type 'OtherFileNonconforming' does not conform to protocol 'VectorProtocol'}}
// expected-error @+1 {{implementation of 'VectorProtocol' cannot be automatically synthesized in an extension in a different file to the type}}
extension OtherFileNonconforming : VectorProtocol {}

// expected-error @+2 {{type 'GenericOtherFileNonconforming<T>' does not conform to protocol 'VectorProtocol'}}
// expected-error @+1 {{implementation of 'VectorProtocol' cannot be automatically synthesized in an extension in a different file to the type}}
extension GenericOtherFileNonconforming : VectorProtocol {}
