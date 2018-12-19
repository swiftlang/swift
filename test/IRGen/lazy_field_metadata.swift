// RUN: %target-swift-frontend -emit-ir -wmo -O %s | %FileCheck %s

// Both should be emitted:

// CHECK: @"$s19lazy_field_metadata011GenericWithD5FieldVMn" = hidden constant
// CHECK: @"$s19lazy_field_metadata24GenericWithConcreteFieldVMn" = hidden constant

struct GenericWithConcreteField<T> {
  let z = 123
}

struct GenericWithGenericField<T> {
  var field = GenericWithConcreteField<T>()
}

public func forceMetadata() -> Any.Type {
  return GenericWithGenericField<Int>.self
}