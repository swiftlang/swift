// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -module-name Test | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-SKIP
// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -module-name Test -experimental-skip-non-exportable-decls | %FileCheck %s --check-prefixes=CHECK,CHECK-SKIP

// CHECK-NO-SKIP: sil private{{.*}} @$s4Test11privateFunc33_E3F0E1C7B46D05C8067CB98677DE566CLLyyF : $@convention(thin) () -> () {
// CHECK-SKIP-NOT: s4Test11privateFunc33_E3F0E1C7B46D05C8067CB98677DE566CLLyyF
private func privateFunc() {}

// CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test12internalFuncyyF : $@convention(thin) () -> () {
// CHECK-SKIP-NOT: s4Test12internalFuncyyF
internal func internalFunc() {}

// CHECK: sil{{.*}} @$s4Test10publicFuncyyF : $@convention(thin) () -> () {
public func publicFunc() {}

private class PrivateClass {
  // CHECK-NO-SKIP: sil private{{.*}} @$s4Test12PrivateClass33_E3F0E1C7B46D05C8067CB98677DE566CLLCfd : $@convention(method) (@guaranteed PrivateClass) -> @owned Builtin.NativeObject {
  // CHECK-SKIP-NOT: s4Test12PrivateClass33_E3F0E1C7B46D05C8067CB98677DE566CLLCfd

  // CHECK-NO-SKIP: sil private{{.*}} @$s4Test12PrivateClass33_E3F0E1C7B46D05C8067CB98677DE566CLLCfD : $@convention(method) (@owned PrivateClass) -> () {
  // CHECK-SKIP-NOT: s4Test12PrivateClass33_E3F0E1C7B46D05C8067CB98677DE566CLLCfD

  // CHECK-NO-SKIP: sil private{{.*}} @$s4Test12PrivateClass33_E3F0E1C7B46D05C8067CB98677DE566CLLCADycfC : $@convention(method) (@thick PrivateClass.Type) -> @owned PrivateClass {
  // CHECK-SKIP-NOT: s4Test12PrivateClass33_E3F0E1C7B46D05C8067CB98677DE566CLLCADycfC

  // CHECK-NO-SKIP: sil private{{.*}} @$s4Test12PrivateClass33_E3F0E1C7B46D05C8067CB98677DE566CLLCADycfc : $@convention(method) (@owned PrivateClass) -> @owned PrivateClass {
  // CHECK-SKIP-NOT: s4Test12PrivateClass33_E3F0E1C7B46D05C8067CB98677DE566CLLCADycfc
}

public class PublicClass {
  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassC14internalMethodyyF : $@convention(method) (@guaranteed PublicClass) -> () {
  // CHECK-SKIP-NOT: s4Test11PublicClassC14internalMethodyyF
  internal func internalMethod() {}

  // CHECK: sil{{.*}} @$s4Test11PublicClassCfd : $@convention(method) (@guaranteed PublicClass) -> @owned Builtin.NativeObject {

  // CHECK: sil{{.*}} @$s4Test11PublicClassCfD : $@convention(method) (@owned PublicClass) -> () {

  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassCACycfC : $@convention(method) (@thick PublicClass.Type) -> @owned PublicClass {
  // CHECK-SKIP-NOT: s4Test11PublicClassCACycfC

  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassCACycfc : $@convention(method) (@owned PublicClass) -> @owned PublicClass {
  // CHECK-SKIP-NOT: s4Test11PublicClassCACycfc
}

extension PublicClass {
  // CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test11PublicClassC25internalMethodInExtensionyyF : $@convention(method) (@guaranteed PublicClass) -> () {
  // CHECK-SKIP-NOT: s4Test11PublicClassC25internalMethodInExtensionyyF
  internal func internalMethodInExtension() {}
}

// CHECK-NO-SKIP-LABEL: sil_vtable PrivateClass {
// CHECK-NO-SKIP-NEXT:    #PrivateClass.init!allocator
// CHECK-NO-SKIP-NEXT:    #PrivateClass.deinit!deallocator
// CHECK-NO-SKIP-NEXT:  }
// CHECK-SKIP-NOT:      sil_vtable PrivateClass

// CHECK-LABEL:         sil_vtable [serialized] PublicClass {
// CHECK-NO-SKIP-NEXT:    #PublicClass.internalMethod
// CHECK-SKIP-NOT:        #PublicClass.internalMethod
// CHECK-NO-SKIP-NEXT:    #PublicClass.init!allocator
// CHECK-SKIP-NOT:        #PublicClass.init!allocator
// CHECK-NEXT:            #PublicClass.deinit!deallocator
// CHECK-NEXT:          }
