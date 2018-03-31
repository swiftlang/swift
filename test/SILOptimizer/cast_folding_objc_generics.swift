
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name cast_folding_objc_generics -O -emit-sil %s | %FileCheck %s
// REQUIRES: objc_interop

import objc_generics

// CHECK-LABEL: sil shared [noinline] @$S26cast_folding_objc_generics26testObjCGenericParamChangeySo12GenericClassCySo8NSStringCGADySo15NSMutableStringCGFTf4n_g : $@convention(thin) (@guaranteed GenericClass<NSMutableString>) -> GenericClass<NSString> {
// CHECK:         upcast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChange(_ a: GenericClass<NSMutableString>) -> GenericClass<NSString> {
  return a as! GenericClass<NSString>
}

// CHECK-LABEL: sil shared [noinline] @$S26cast_folding_objc_generics34testObjCGenericParamChangeSubclassySo07GenericJ0CySo8NSStringCGSo0K5ClassCySo15NSMutableStringCGFTf4n_g : $@convention(thin) (@guaranteed GenericClass<NSMutableString>) -> GenericSubclass<NSString> {
// CHECK:         unconditional_checked_cast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChangeSubclass(_ a: GenericClass<NSMutableString>) -> GenericSubclass<NSString> {
  return a as! GenericSubclass<NSString>
}

// CHECK-LABEL: sil shared [noinline] @$S26cast_folding_objc_generics36testObjCGenericParamChangeSuperclassySo12GenericClassCySo8NSStringCGSo0K8SubclassCySo15NSMutableStringCGFTf4n_g : $@convention(thin) (@guaranteed GenericSubclass<NSMutableString>) -> GenericClass<NSString> {
// CHECK:         upcast
// CHECK-NOT:     int_trap
@inline(never)
public func testObjCGenericParamChangeSuperclass(_ a: GenericSubclass<NSMutableString>) -> GenericClass<NSString> {
  return a as! GenericClass<NSString>
}
