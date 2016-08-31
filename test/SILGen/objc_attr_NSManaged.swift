// RUN: %target-swift-frontend -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

// This file is also used by objc_attr_NSManaged_multi.swift.

import Foundation
import gizmo

@objc class X : NSObject {
  func foo() -> X { return self }
}

class SwiftGizmo : Gizmo {
  @NSManaged var x: X

  @NSManaged func kvc()

  // CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmog1x
  // CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmos1x
  // CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmo3kvc

  // Make sure that we're calling through the @objc entry points.
  // CHECK-LABEL: sil hidden @_TFC19objc_attr_NSManaged10SwiftGizmo7modifyX{{.*}} : $@convention(method) (@guaranteed SwiftGizmo) -> () {
  func modifyX() {
    // CHECK:   [[GETTER:%[0-9]+]] = class_method [volatile] [[SELF:%.*]] : $SwiftGizmo, #SwiftGizmo.x!getter.1.foreign : (SwiftGizmo) -> () -> X , $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
    // CHECK-NEXT: apply [[GETTER]]([[SELF]]) : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
    // CHECK-NOT: return
    // CHECK:   [[SETTER:%[0-9]+]] = class_method [volatile] [[SELF]] : $SwiftGizmo, #SwiftGizmo.x!setter.1.foreign : (SwiftGizmo) -> (X) -> () , $@convention(objc_method) (X, SwiftGizmo) -> ()
    // CHECK:  apply [[SETTER]]([[XMOD:%.*]], [[SELF]]) : $@convention(objc_method) (X, SwiftGizmo) -> ()
    x = x.foo()
    // CHECK: return
  }

  // CHECK-LABEL: sil hidden @_TFC19objc_attr_NSManaged10SwiftGizmo8testFunc
  func testFunc() {
    // CHECK: = class_method [volatile] %0 : $SwiftGizmo, #SwiftGizmo.kvc!1.foreign : (SwiftGizmo) -> () -> () , $@convention(objc_method) (SwiftGizmo) -> ()
    // CHECK: return
    kvc()
  }
}

extension SwiftGizmo {
  @NSManaged func extKVC()

  // CHECK-LABEL: _TFC19objc_attr_NSManaged10SwiftGizmo7testExt
  func testExt() {
    // CHECK: = class_method [volatile] %0 : $SwiftGizmo, #SwiftGizmo.extKVC!1.foreign : (SwiftGizmo) -> () -> () , $@convention(objc_method) (SwiftGizmo) -> ()
    // CHECK: return
    extKVC()
  }
}

final class FinalGizmo : SwiftGizmo {
  @NSManaged var y: String

  @NSManaged func kvc2()
}

extension FinalGizmo {
  @NSManaged func extKVC2()

  // CHECK-LABEL: _TFC19objc_attr_NSManaged10FinalGizmo8testExt2
  func testExt2() {
    // CHECK: = class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.extKVC2!1.foreign : (FinalGizmo) -> () -> () , $@convention(objc_method) (FinalGizmo) -> ()
    // CHECK: return
    extKVC2()
  }
}

// CHECK-LABEL: sil hidden @_TF19objc_attr_NSManaged9testFinalFCS_10FinalGizmoSS : $@convention(thin) (@owned FinalGizmo) -> @owned String {
func testFinal(_ obj: FinalGizmo) -> String {
  // CHECK: class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.kvc2!1.foreign : (FinalGizmo) -> () -> () , $@convention(objc_method) (FinalGizmo) -> ()
  // CHECK-NOT: return
  // CHECK: class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.y!getter.1.foreign : (FinalGizmo) -> () -> String , $@convention(objc_method) (FinalGizmo) -> @autoreleased NSString
  // CHECK: return
  obj.kvc2()
  return obj.y
}


// CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmog1xCS_1X : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
// CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10SwiftGizmos1xCS_1X
// CHECK-NOT: sil hidden @_TToFC19objc_attr_NSManaged10FinalGizmog1y


// The vtable should not contain any entry points for getters and setters.
// CHECK-LABEL: sil_vtable SwiftGizmo {
// CHECK-NEXT: #SwiftGizmo.modifyX!1: _TFC19objc_attr_NSManaged10SwiftGizmo7modifyX
// CHECK-NEXT: #SwiftGizmo.testFunc!1: _TFC19objc_attr_NSManaged10SwiftGizmo8testFunc
// CHECK-NEXT:  #SwiftGizmo.init!initializer.1: _TFC19objc_attr_NSManaged10SwiftGizmoc
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: _TFC19objc_attr_NSManaged10SwiftGizmoc
// CHECK-NEXT:  #SwiftGizmo.deinit!deallocator:
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable FinalGizmo {
// CHECK-NEXT:   #SwiftGizmo.modifyX!1: _TFC19objc_attr_NSManaged10SwiftGizmo7modifyX
// CHECK-NEXT: #SwiftGizmo.testFunc!1: _TFC19objc_attr_NSManaged10SwiftGizmo8testFunc
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: _TFC19objc_attr_NSManaged10FinalGizmoc
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: _TFC19objc_attr_NSManaged10FinalGizmoc
// CHECK-NEXT:   #FinalGizmo.deinit!deallocator: _TFC19objc_attr_NSManaged10FinalGizmoD
// CHECK-NEXT: }
