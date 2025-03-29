
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name objc_attr_NSManaged_multi -sdk %S/Inputs -primary-file %s %S/objc_attr_NSManaged.swift -I %S/Inputs -enable-source-import | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden [ossa] @$s25objc_attr_NSManaged_multi9testMultiyyXlAA10SwiftGizmoCF : $@convention(thin) (@guaranteed SwiftGizmo) -> @owned AnyObject {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $SwiftGizmo):
// CHECK: = objc_method [[ARG]] : $SwiftGizmo, #SwiftGizmo.kvc!foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
// CHECK-NOT: return
// CHECK: = objc_method [[ARG]] : $SwiftGizmo, #SwiftGizmo.extKVC!foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
// CHECK-NOT: return
// CHECK: objc_method [[ARG]] : $SwiftGizmo, #SwiftGizmo.x!getter.foreign : (SwiftGizmo) -> () -> X, $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
// CHECK: return
func testMulti(_ obj: SwiftGizmo) -> AnyObject {
  obj.kvc()
  obj.extKVC()
  return obj.x
}

// CHECK-LABEL: sil hidden [ossa] @$s25objc_attr_NSManaged_multi14testFinalMultiySSAA0F5GizmoCF : $@convention(thin) (@guaranteed FinalGizmo) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $FinalGizmo):
// CHECK: objc_method [[ARG]] : $FinalGizmo, #FinalGizmo.kvc2!foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
// CHECK-NOT: return
// CHECK: objc_method [[ARG]] : $FinalGizmo, #FinalGizmo.extKVC2!foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
// CHECK-NOT: return
// CHECK: objc_method [[ARG]] : $FinalGizmo, #FinalGizmo.y!getter.foreign : (FinalGizmo) -> () -> String, $@convention(objc_method) (FinalGizmo) -> @autoreleased NSString
// CHECK: return
func testFinalMulti(_ obj: FinalGizmo) -> String {
  obj.kvc2()
  obj.extKVC2()
  return obj.y
}
