// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -wmo -O %s > %t/out.txt
// RUN: %FileCheck %s < %t/out.txt
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t/out.txt

// Both should be emitted:

// CHECK: @"$s19lazy_field_metadata011GenericWithD5FieldVMn" = hidden constant
// CHECK: @"$s19lazy_field_metadata24GenericWithConcreteFieldVMn" = hidden constant

struct GenericWithConcreteField<T> {
  let z = 123
}

struct GenericWithGenericField<T> {
  var field = GenericWithConcreteField<T>()
}

@_optimize(none)
public func takeMetadata<T>(_: T.Type) {}

public func forceMetadata() {
  takeMetadata(GenericWithGenericField<Int>.self)
}

// These two types are completely unused.
struct TypeOfProperty {
  var x: Int
}

struct HasPropertyType {
  var p: TypeOfProperty
}

// NEGATIVE-NOT: @"$s19lazy_field_metadata14TypeOfPropertyVMn"
// NEGATIVE-NOT: @"$s19lazy_field_metadata15HasPropertyTypeVMn"
