// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: env %env-SWIFT_DEBUG_VALIDATE_EXTERNAL_GENERIC_METADATA_BUILDER=2 %target-run %t/a.out 2>&1 | %FileCheck %s

// The builder doesn't yet know how to look up symbols on Windows.
// UNSUPPORTED: OS=windows-msvc

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Validation gets confused by ARM64e, temporarily disable the test there until
// we've fixed that. rdar://121029024
// UNSUPPORTED: CPU=arm64e

struct ConcreteEmpty {}

struct ConcreteFields {
  var a: Int
  var b: String
  var c: Double
}

struct Empty<T> {}

struct OneField<T> {
  var field: T
}

struct DerivedField<T> {
  var field: OneField<T>
}

struct MultipleGenericTypes<T, U, V, W> {
  var t: T
  var u: OneField<U>
  var v: DerivedField<W>
}

struct Nested1<T, U> {
  struct Nested2<V, W> {
    struct Nested3<X, Y> {
      var t: T
      var u: U
      var v: V
      var w: W
      var x: X
      var y: Y
    }
  }
}

// Force the given metadata to be instantiated. Print the identifier as a side
// effect that requires instantiation and can't be optimized away.
func testMetadata(_ t: Any.Type, line: UInt = #line) {
  print("line \(line) - \(t) \(ObjectIdentifier(t))")
}

testMetadata(Empty<Int>.self)
testMetadata(Empty<ConcreteEmpty>.self)
testMetadata(Empty<Empty<Empty<ConcreteEmpty>>>.self)
testMetadata(Empty<Empty<Empty<Int>>>.self)
// CHECK: Validated generic metadata builder on Empty<Int>
// CHECK: Validated generic metadata builder on Empty<ConcreteEmpty>
// CHECK: Validated generic metadata builder on Empty<Empty<Empty<ConcreteEmpty>>>
// CHECK: Validated generic metadata builder on Empty<Empty<Empty<Int>>>

testMetadata(OneField<Int>.self)
testMetadata(OneField<ConcreteEmpty>.self)
testMetadata(OneField<OneField<OneField<ConcreteEmpty>>>.self)
testMetadata(OneField<OneField<OneField<Int>>>.self)
// CHECK: Validated generic metadata builder on OneField<Int>
// CHECK: Validated generic metadata builder on OneField<ConcreteEmpty>
// CHECK: Validated generic metadata builder on OneField<OneField<OneField<ConcreteEmpty>>>
// CHECK: Validated generic metadata builder on OneField<OneField<OneField<Int>>>

testMetadata(DerivedField<Int>.self)
testMetadata(DerivedField<ConcreteEmpty>.self)
testMetadata(DerivedField<DerivedField<DerivedField<ConcreteEmpty>>>.self)
testMetadata(DerivedField<DerivedField<DerivedField<Int>>>.self)
// CHECK: Validated generic metadata builder on DerivedField<Int>
// CHECK: Validated generic metadata builder on DerivedField<ConcreteEmpty>
// CHECK: Validated generic metadata builder on DerivedField<DerivedField<DerivedField<ConcreteEmpty>>>
// CHECK: Validated generic metadata builder on DerivedField<DerivedField<DerivedField<Int>>>

testMetadata(DerivedField<OneField<Int>>.self)
testMetadata(DerivedField<OneField<ConcreteEmpty>>.self)
// CHECK: Validated generic metadata builder on DerivedField<OneField<Int>>
// CHECK: Validated generic metadata builder on DerivedField<OneField<ConcreteEmpty>>

testMetadata(MultipleGenericTypes<Int, Double, Float, String>.self)
testMetadata(MultipleGenericTypes<DerivedField<Int>, OneField<Float>, Empty<String>, String>.self)
testMetadata(Nested1<OneField<Int>, DerivedField<Double>>.Nested2<Int, String>.Nested3<Empty<Int>, OneField<OneField<Int>>>.self)
// CHECK: Validated generic metadata builder on MultipleGenericTypes<Int, Double, Float, String>
// CHECK: Validated generic metadata builder on MultipleGenericTypes<DerivedField<Int>, OneField<Float>, Empty<String>, String>
// CHECK: Validated generic metadata builder on Nested3<Empty<Int>, OneField<OneField<Int>
