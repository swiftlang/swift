// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -O -emit-sil %s | %FileCheck %s
// REQUIRES: objc_interop

import objc_generics

// CHECK-LABEL: sil [noinline] @_TF26cast_folding_objc_generics26testObjCGenericParamChange
// CHECK:         upcast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChange(_ a: GenericClass<NSMutableString>) -> GenericClass<NSString> {
  return a as! GenericClass<NSString>
}

// CHECK-LABEL: sil [noinline] @_TF26cast_folding_objc_generics34testObjCGenericParamChangeSubclass
// CHECK:         unconditional_checked_cast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChangeSubclass(_ a: GenericClass<NSMutableString>) -> GenericSubclass<NSString> {
  return a as! GenericSubclass<NSString>
}

// CHECK-LABEL: sil [noinline] @_TF26cast_folding_objc_generics36testObjCGenericParamChangeSuperclass
// CHECK:         upcast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChangeSuperclass(_ a: GenericSubclass<NSMutableString>) -> GenericClass<NSString> {
  return a as! GenericClass<NSString>
}
