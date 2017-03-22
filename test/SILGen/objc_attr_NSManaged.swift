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

  // CHECK-NOT: sil hidden @_T019objc_attr_NSManaged10SwiftGizmoC1x{{[_0-9a-zA-Z]*}}fgTo
  // CHECK-NOT: sil hidden @_T019objc_attr_NSManaged10SwiftGizmoC1x{{[_0-9a-zA-Z]*}}fsTo
  // CHECK-NOT: sil hidden @_T019objc_attr_NSManaged10SwiftGizmoC3kvc{{[_0-9a-zA-Z]*}}FTo

  // Make sure that we're calling through the @objc entry points.
  // CHECK-LABEL: sil hidden @_T019objc_attr_NSManaged10SwiftGizmoC7modifyX{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed SwiftGizmo) -> () {
  func modifyX() {
    // CHECK:   [[GETTER:%[0-9]+]] = class_method [volatile] [[SELF:%.*]] : $SwiftGizmo, #SwiftGizmo.x!getter.1.foreign : (SwiftGizmo) -> () -> X, $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
    // CHECK-NEXT: apply [[GETTER]]([[SELF]]) : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
    // CHECK-NOT: return
    // CHECK:   [[SETTER:%[0-9]+]] = class_method [volatile] [[SELF]] : $SwiftGizmo, #SwiftGizmo.x!setter.1.foreign : (SwiftGizmo) -> (X) -> (), $@convention(objc_method) (X, SwiftGizmo) -> ()
    // CHECK:  apply [[SETTER]]([[XMOD:%.*]], [[SELF]]) : $@convention(objc_method) (X, SwiftGizmo) -> ()
    x = x.foo()
    // CHECK: return
  }

  // CHECK-LABEL: sil hidden @_T019objc_attr_NSManaged10SwiftGizmoC8testFunc{{[_0-9a-zA-Z]*}}F
  func testFunc() {
    // CHECK: = class_method [volatile] %0 : $SwiftGizmo, #SwiftGizmo.kvc!1.foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
    // CHECK: return
    kvc()
  }
}

extension SwiftGizmo {
  @NSManaged func extKVC()

  // CHECK-LABEL: _T019objc_attr_NSManaged10SwiftGizmoC7testExt{{[_0-9a-zA-Z]*}}F
  func testExt() {
    // CHECK: = class_method [volatile] %0 : $SwiftGizmo, #SwiftGizmo.extKVC!1.foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
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

  // CHECK-LABEL: _T019objc_attr_NSManaged10FinalGizmoC8testExt2{{[_0-9a-zA-Z]*}}F
  func testExt2() {
    // CHECK: = class_method [volatile] %0 : $FinalGizmo, #FinalGizmo.extKVC2!1.foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
    // CHECK: return
    extKVC2()
  }
}

// CHECK-LABEL: sil hidden @_T019objc_attr_NSManaged9testFinalSSAA0E5GizmoCF : $@convention(thin) (@owned FinalGizmo) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : $FinalGizmo):
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $FinalGizmo, #FinalGizmo.kvc2!1.foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
// CHECK-NOT: return
// CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK: class_method [volatile] [[BORROWED_ARG]] : $FinalGizmo, #FinalGizmo.y!getter.1.foreign : (FinalGizmo) -> () -> String, $@convention(objc_method) (FinalGizmo) -> @autoreleased NSString
// CHECK: return
func testFinal(_ obj: FinalGizmo) -> String {
  obj.kvc2()
  return obj.y
}

// SR-2673: @NSManaged property can't satisfy protocol requirement
@objc protocol ObjCProto {
  var managedProp: String { get set }
  var managedExtProp: AnyObject { get }
}

class ProtoAdopter: Gizmo, ObjCProto {
  @NSManaged var managedProp: String
}
extension ProtoAdopter {
  @NSManaged var managedExtProp: AnyObject
}


// CHECK-NOT: sil hidden @_T019objc_attr_NSManaged10SwiftGizmoC1xAA1XCfgTo : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
// CHECK-NOT: sil hidden @_T019objc_attr_NSManaged10SwiftGizmoC1xAA1XCfsTo
// CHECK-NOT: sil hidden @_T019objc_attr_NSManaged10{{[_0-9a-zA-Z]*}}FinalGizmoC1yytfgTo


// The vtable should not contain any entry points for getters and setters.
// CHECK-LABEL: sil_vtable SwiftGizmo {
// CHECK-NEXT:   #SwiftGizmo.modifyX!1: {{.*}} : _T019objc_attr_NSManaged10SwiftGizmoC7modifyXyyF
// CHECK-NEXT:   #SwiftGizmo.testFunc!1: {{.*}} : _T019objc_attr_NSManaged10SwiftGizmoC8testFuncyyF
// CHECK-NEXT:   #SwiftGizmo.deinit!deallocator: _T019objc_attr_NSManaged10SwiftGizmoCfD
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: {{.*}} : _T019objc_attr_NSManaged10SwiftGizmoCSQyACGycfc
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: {{.*}} : _T019objc_attr_NSManaged10SwiftGizmoCSQyACGSi7bellsOn_tcfc
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable FinalGizmo {
// CHECK-NEXT:   #SwiftGizmo.modifyX!1: {{.*}} : _T019objc_attr_NSManaged10SwiftGizmoC7modifyX{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #SwiftGizmo.testFunc!1: {{.*}} : _T019objc_attr_NSManaged10SwiftGizmoC8testFunc{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: {{.*}} : _T019objc_attr_NSManaged10FinalGizmoC{{[_0-9a-zA-Z]*}}fc
// CHECK-NEXT:   #SwiftGizmo.init!initializer.1: {{.*}} : _T019objc_attr_NSManaged10FinalGizmoC{{[_0-9a-zA-Z]*}}fc
// CHECK-NEXT:   #FinalGizmo.deinit!deallocator: _T019objc_attr_NSManaged10FinalGizmoCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable ProtoAdopter {
// CHECK-NOT: managed{{.*}}Prop
// CHECK: {{^}$}}
