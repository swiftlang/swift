// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_elementary_functions_other_module.swift

struct Empty : ElementaryFunctions {}
func testEmpty() {
  _ = Empty()
}

struct Float2: ElementaryFunctions {
  let a: Float
  var b: Float
}
func testFloat2() {
  _ = Float2(a: 1, b: 1)
}

// Test generic type.
struct Vector2<T : ElementaryFunctions>: ElementaryFunctions {
  let x: T
  var y: T
}
func testVector2() {
  _ = Vector2<Double>(x: 1, y: 1)
}

// Test nested type.
struct Nested: ElementaryFunctions {
  let float2: Float2
  var float: Float
}
func testNested(float2: Float2) {
  _ = Nested(float2: float2, float: 1)
}

// Test mixed type.
struct Mixed: ElementaryFunctions {
  let nested: Nested
  var float = Float(1)
  var double: Double
}
func testMixed(nested: Nested) {
  _ = Mixed(nested: nested, float: 1, double: 1)
}

// Test type in generic context.
struct A<T> {
  struct B<U, V> {
    struct GenericContextNested : ElementaryFunctions {
      var nested: Nested
      let float: Float
      var double = Double(2)
    }
  }
}
func testGenericContext<T, U, V>(nested: Nested) -> A<T>.B<U, V>.GenericContextNested {
  A<T>.B<U, V>.GenericContextNested(nested: nested, float: 1, double: 1)
}

// Test extension.
struct Extended {
  var x: Float
}
extension Extended : ElementaryFunctions {}

// Test extension of generic type.
struct GenericExtended<T> {
  var x: T
}
extension GenericExtended : ElementaryFunctions where T : ElementaryFunctions {}

// Test memberwise initializer synthesis.
struct NoMemberwiseInitializer<T : ElementaryFunctions> : ElementaryFunctions {
  var value: T
  init(randomLabel value: T) { self.value = value }
}
struct NoMemberwiseInitializerExtended<T> {
  var value: T
  init(_ value: T) {
    self.value = value
  }
}
extension NoMemberwiseInitializerExtended: ElementaryFunctions
  where T : ElementaryFunctions {}

// Test derived conformances in disallowed contexts.

// expected-error @+1 24 {{implementation of 'ElementaryFunctions' cannot be automatically synthesized in an extension in a different file to the type}}
extension OtherFileNonconforming : ElementaryFunctions {}

// expected-error @+1 24 {{implementation of 'ElementaryFunctions' cannot be automatically synthesized in an extension in a different file to the type}}
extension GenericOtherFileNonconforming : ElementaryFunctions {}
