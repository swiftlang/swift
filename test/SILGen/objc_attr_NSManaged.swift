// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

// This file is also used by objc_attr_NSManaged_multi.swift.

import Foundation
import gizmo

@objc class X : NSObject {
  func foo() -> X { return self }
}

class SwiftGizmo : Gizmo {
  @NSManaged var x: X

  // CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmog1xCS_1X : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
  // CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmos1xCS_1X

  // Make sure that we're calling through the @objc entry points.
  // CHECK-LABEL: sil hidden @_TFC19objc_attr_NSManaged10SwiftGizmo7modifyX{{.*}} : $@convention(method) (@guaranteed SwiftGizmo) -> () {
  func modifyX() {
    // CHECK:   [[GETTER:%[0-9]+]] = class_method [volatile] [[SELF:%.*]] : $SwiftGizmo, #SwiftGizmo.x!getter.1.foreign : SwiftGizmo -> () -> X , $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
    // CHECK-NEXT: apply [[GETTER]]([[SELF]]) : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
    // CHECK-NOT: return
    // CHECK:   [[SETTER:%[0-9]+]] = class_method [volatile] [[SELF]] : $SwiftGizmo, #SwiftGizmo.x!setter.1.foreign : SwiftGizmo -> (X) -> () , $@convention(objc_method) (X, SwiftGizmo) -> ()
    // CHECK:  apply [[SETTER]]([[XMOD:%.*]], [[SELF]]) : $@convention(objc_method) (X, SwiftGizmo) -> ()
    x = x.foo()
    // CHECK: return
  }
}

final class FinalGizmo : SwiftGizmo {
  @NSManaged var y: String
}

// CHECK-LABEL: sil hidden @_TF19objc_attr_NSManaged9testFinalFCS_10FinalGizmoSS : $@convention(thin) (@owned FinalGizmo) -> @owned String {
func testFinal(obj: FinalGizmo) -> String {
  // CHECK: class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.y!getter.1.foreign : FinalGizmo -> () -> String , $@convention(objc_method) (FinalGizmo) -> @autoreleased NSString
  // CHECK: return
  return obj.y
}


// CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmog1xCS_1X : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
// CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmos1xCS_1X
// CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10FinalGizmog1y


// The vtable should not contain any entry points for getters and setters.
// CHECK-LABEL: sil_vtable SwiftGizmo {
// CHECK-NEXT: #SwiftGizmo.modifyX!1: _TFC19objc_attr_NSManaged10SwiftGizmo7modifyXfS0_FT_T_ // objc_attr_NSManaged.SwiftGizmo.modifyX (objc_attr_NSManaged.SwiftGizmo)() -> ()
// CHECK-NEXT:  #SwiftGizmo.deinit!deallocator:
// CHECK-NEXT:  #SwiftGizmo.init!initializer.1: _TFC19objc_attr_NSManaged10SwiftGizmocfMS0_FT_GSQS0__
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: _TFC19objc_attr_NSManaged10SwiftGizmocfMS0_FT7bellsOnSi_GSQS0__
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable FinalGizmo {
// CHECK-NEXT:   #SwiftGizmo.modifyX!1: _TFC19objc_attr_NSManaged10SwiftGizmo7modifyXfS0_FT_T_
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: _TFC19objc_attr_NSManaged10FinalGizmocfMS0_FT_GSQS0__
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: _TFC19objc_attr_NSManaged10FinalGizmocfMS0_FT7bellsOnSi_GSQS0__
// CHECK-NEXT:   #FinalGizmo.deinit!deallocator: _TFC19objc_attr_NSManaged10FinalGizmoD
// CHECK-NEXT: }
