// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -Xllvm -new-mangling-for-tests -O -emit-sil %s | %FileCheck %s
// REQUIRES: objc_interop

import objc_generics

// CHECK-LABEL: sil [noinline] @_T026cast_folding_objc_generics26testObjCGenericParamChange{{[_0-9a-zA-Z]*}}F
// CHECK:         upcast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChange(_ a: GenericClass<NSMutableString>) -> GenericClass<NSString> {
  return a as! GenericClass<NSString>
}

// CHECK-LABEL: sil [noinline] @_T026cast_folding_objc_generics34testObjCGenericParamChangeSubclass{{[_0-9a-zA-Z]*}}F
// CHECK:         unconditional_checked_cast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChangeSubclass(_ a: GenericClass<NSMutableString>) -> GenericSubclass<NSString> {
  return a as! GenericSubclass<NSString>
}

// CHECK-LABEL: sil [noinline] @_T026cast_folding_objc_generics36testObjCGenericParamChangeSuperclass{{[_0-9a-zA-Z]*}}F
// CHECK:         upcast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChangeSuperclass(_ a: GenericSubclass<NSMutableString>) -> GenericClass<NSString> {
  return a as! GenericClass<NSString>
}
