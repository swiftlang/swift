// RUN: rm -rf %t && mkdir %t

// RUN: %target-swift-frontend -sdk "" -I %S/Inputs/objc_xref_new/ -emit-module -o %t %S/Inputs/objc_xref_new/Other.swift -enable-experimental-objc-xrefs
// RUN: %target-swift-frontend -sdk "" -I %S/Inputs/objc_xref_new/ -I %t -emit-silgen %s -enable-experimental-objc-xrefs > /dev/null
// RUN: %target-swift-frontend -sdk "" -I %S/Inputs/objc_xref_new/ -I %t -emit-silgen %s -DNEW -Xcc -DNEW -enable-experimental-objc-xrefs | %FileCheck %s

// REQUIRES: objc_interop

import ObjCModule
import Other

// CHECK-LABEL: sil hidden @_TF13objc_xref_new4testFT3subC5Other3Sub_T_
func test(sub: Sub) {
  // CHECK: class_method %0 : $Sub, #Sub.testBaseNameChanges!1 : (Sub) -> () -> ()
  sub.testBaseNameChanges()
  // CHECK: class_method %0 : $Sub, #Sub.testArgumentNameChanges!1 : (Sub) -> (Any, Any) -> ()
  sub.testArgumentNameChanges(sub, next: sub)
  // CHECK: class_method %0 : $Sub, #Sub.testNullabilityArgChanges!1 : (Sub) -> (Any) -> ()
  sub.testNullabilityArgChanges(sub)
  // CHECK: class_method %0 : $Sub, #Sub.testNullabilityReturnChanges!1 : (Sub) -> () -> Any
  _ = sub.testNullabilityReturnChanges()
}

#if NEW
// CHECK-LABEL: sil hidden @_TF13objc_xref_new4testFT3objCSo4Base_T_
func test(obj: Base) {
  // CHECK: class_method [volatile] %0 : $Base, #Base.testNewBaseName!1.foreign : (Base) -> () -> ()
  obj.testNewBaseName()
  // CHECK: class_method [volatile] %0 : $Base, #Base.testArgumentNameChanges!1.foreign : (Base) -> (Any, Any) -> ()
  obj.testArgumentNameChanges(first: obj, second: obj)
  // CHECK: class_method [volatile] %0 : $Base, #Base.testNullabilityArgChanges!1.foreign : (Base) -> (Any?) -> ()
  obj.testNullabilityArgChanges(obj)
  // CHECK: class_method [volatile] %0 : $Base, #Base.testNullabilityReturnChanges!1.foreign : (Base) -> () -> Any?
  _ = obj.testNullabilityReturnChanges()
}
#endif
