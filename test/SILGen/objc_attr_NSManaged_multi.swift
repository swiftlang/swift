// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -sdk %S/Inputs -primary-file %s %S/objc_attr_NSManaged.swift -I %S/Inputs -enable-source-import -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_T025objc_attr_NSManaged_multi9testMultis9AnyObject_pAA10SwiftGizmoCF : $@convention(thin) (@owned SwiftGizmo) -> @owned AnyObject {
func testMulti(_ obj: SwiftGizmo) -> AnyObject {
  // CHECK: = class_method [volatile] %0 : $SwiftGizmo, #SwiftGizmo.kvc!1.foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
  // CHECK-NOT: return
  // CHECK: = class_method [volatile] %0 : $SwiftGizmo, #SwiftGizmo.extKVC!1.foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
  // CHECK-NOT: return
  // CHECK: class_method [volatile] %0 : $SwiftGizmo, #SwiftGizmo.x!getter.1.foreign : (SwiftGizmo) -> () -> X, $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
  // CHECK: return
  obj.kvc()
  obj.extKVC()
  return obj.x
}

// CHECK-LABEL: sil hidden @_T025objc_attr_NSManaged_multi14testFinalMultiSSAA0F5GizmoCF : $@convention(thin) (@owned FinalGizmo) -> @owned String {
func testFinalMulti(_ obj: FinalGizmo) -> String {
  // CHECK: class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.kvc2!1.foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
  // CHECK-NOT: return
  // CHECK: class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.extKVC2!1.foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
  // CHECK-NOT: return
  // CHECK: class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.y!getter.1.foreign : (FinalGizmo) -> () -> String, $@convention(objc_method) (FinalGizmo) -> @autoreleased NSString
  // CHECK: return
  obj.kvc2()
  obj.extKVC2()
  return obj.y
}
