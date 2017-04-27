// RUN: %target-swift-frontend -sdk %S/Inputs -primary-file %s %S/objc_attr_NSManaged.swift -I %S/Inputs -enable-source-import -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_T025objc_attr_NSManaged_multi9testMultis9AnyObject_pAA10SwiftGizmoCF : $@convention(thin) (@owned SwiftGizmo) -> @owned AnyObject {
// CHECK: bb0([[ARG:%.*]] : $SwiftGizmo):
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: = class_method [volatile] [[BORROWED_ARG]] : $SwiftGizmo, #SwiftGizmo.kvc!1.foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
// CHECK-NOT: return
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: = class_method [volatile] [[BORROWED_ARG]] : $SwiftGizmo, #SwiftGizmo.extKVC!1.foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
// CHECK-NOT: return
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $SwiftGizmo, #SwiftGizmo.x!getter.1.foreign : (SwiftGizmo) -> () -> X, $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
// CHECK: return
func testMulti(_ obj: SwiftGizmo) -> AnyObject {
  obj.kvc()
  obj.extKVC()
  return obj.x
}

// CHECK-LABEL: sil hidden @_T025objc_attr_NSManaged_multi14testFinalMultiSSAA0F5GizmoCF : $@convention(thin) (@owned FinalGizmo) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : $FinalGizmo):
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $FinalGizmo, #FinalGizmo.kvc2!1.foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
// CHECK-NOT: return
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $FinalGizmo, #FinalGizmo.extKVC2!1.foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
// CHECK-NOT: return
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $FinalGizmo, #FinalGizmo.y!getter.1.foreign : (FinalGizmo) -> () -> String, $@convention(objc_method) (FinalGizmo) -> @autoreleased NSString
// CHECK: return
func testFinalMulti(_ obj: FinalGizmo) -> String {
  obj.kvc2()
  obj.extKVC2()
  return obj.y
}
