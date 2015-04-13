// RUN: %target-swift-frontend -sdk %S/Inputs -primary-file %s %S/objc_attr_NSManaged.swift -I %S/Inputs -enable-source-import -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_TF25objc_attr_NSManaged_multi9testMultiFCS_10SwiftGizmoPSs9AnyObject_ : $@convention(thin) (@owned SwiftGizmo) -> @owned AnyObject {
func testMulti(obj: SwiftGizmo) -> AnyObject {
  // CHECK: class_method [volatile] %0 : $SwiftGizmo, #SwiftGizmo.x!getter.1.foreign : SwiftGizmo -> () -> X , $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
  // CHECK: return
  return obj.x
}

// CHECK-LABEL: sil hidden @_TF25objc_attr_NSManaged_multi14testFinalMultiFCS_10FinalGizmoSS : $@convention(thin) (@owned FinalGizmo) -> @owned String {
func testFinalMulti(obj: FinalGizmo) -> String {
  // CHECK: class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.y!getter.1.foreign : FinalGizmo -> () -> String , $@convention(objc_method) (FinalGizmo) -> @autoreleased NSString
  // CHECK: return
  return obj.y
}
