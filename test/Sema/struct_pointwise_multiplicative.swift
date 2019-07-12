// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_pointwise_multiplicative_other_module.swift

func testPointwiseMultiplicative<T : PointwiseMultiplicative>(
  _ x: inout T
) {
  // Test `PointwiseMultiplicative` requirements: `reciprocal`, `.*`.
  x .*= x .* x.reciprocal
}

struct Empty : PointwiseMultiplicative {}
func testEmpty() {
  var empty = Empty()
  testPointwiseMultiplicative(&empty)
}

// Test generic type.
struct Vector2<T : PointwiseMultiplicative>: PointwiseMultiplicative {
  var x: T
  var y: T
}
func testVector2() {
  var vec2 = Vector2<Empty>(x: Empty(), y: Empty())
  testPointwiseMultiplicative(&vec2)
}

// Test nested type.
struct Nested: PointwiseMultiplicative {
  var empty: Empty
  var vec2: Vector2<Empty>
}
func testNested(vec2: Vector2<Empty>) {
  var nested = Nested(empty: Empty(), vec2: vec2)
  testPointwiseMultiplicative(&nested)
}

// Test type in generic context.
struct A<T> {
  struct B<U, V> {
    struct GenericContextNested : PointwiseMultiplicative {
      var empty: Empty
      var nested: Nested
    }
  }
}
func testGenericContext<T, U, V>(nested: Nested) -> A<T>.B<U, V>.GenericContextNested {
  var genericNested =
    A<T>.B<U, V>.GenericContextNested(empty: Empty(), nested: nested)
  testPointwiseMultiplicative(&genericNested)
  return genericNested
}

// Test extension.
struct Extended {
  var empty: Empty
}
extension Extended : Equatable, AdditiveArithmetic, PointwiseMultiplicative {}

// Test extension of generic type.
struct GenericExtended<T> {
  var x: T
}
extension GenericExtended : Equatable, AdditiveArithmetic, PointwiseMultiplicative
  where T : PointwiseMultiplicative {}

// Test memberwise initializer synthesis.
struct NoMemberwiseInitializer<T : PointwiseMultiplicative> : PointwiseMultiplicative {
  var value: T
  init(randomLabel value: T) { self.value = value }
}
struct NoMemberwiseInitializerCustomOne: PointwiseMultiplicative {
  var x: Empty
  static var one: Self { return NoMemberwiseInitializerCustomOne(Empty()) }
  init(_ x: Empty) {
    self.x = x
  }
}
struct NoMemberwiseInitializerExtended<T> {
  var value: T
  init(_ value: T) {
    self.value = value
  }
}
extension NoMemberwiseInitializerExtended: Equatable, AdditiveArithmetic, PointwiseMultiplicative
  where T : PointwiseMultiplicative {}

// Test derived conformances in disallowed contexts.

// expected-error @+1 2 {{implementation of 'PointwiseMultiplicative' cannot be automatically synthesized in an extension in a different file to the type}}
extension OtherFileNonconforming : PointwiseMultiplicative {}

// expected-error @+1 2 {{implementation of 'PointwiseMultiplicative' cannot be automatically synthesized in an extension in a different file to the type}}
extension GenericOtherFileNonconforming : PointwiseMultiplicative {}
