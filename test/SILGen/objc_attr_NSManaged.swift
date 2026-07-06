
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name objc_attr_NSManaged -sdk %S/Inputs %s -I %S/Inputs -enable-source-import | %FileCheck %s

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

  // CHECK-NOT: sil hidden [ossa] @$s19objc_attr_NSManaged10SwiftGizmoC1x{{[_0-9a-zA-Z]*}}fgTo
  // CHECK-NOT: sil hidden [ossa] @$s19objc_attr_NSManaged10SwiftGizmoC1x{{[_0-9a-zA-Z]*}}fsTo
  // CHECK-NOT: sil hidden [ossa] @$s19objc_attr_NSManaged10SwiftGizmoC3kvc{{[_0-9a-zA-Z]*}}FTo

  // Make sure that we're calling through the @objc entry points.
  // CHECK-LABEL: sil hidden [ossa] @$s19objc_attr_NSManaged10SwiftGizmoC7modifyX{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed SwiftGizmo) -> () {
  func modifyX() {
    // CHECK:   [[GETTER:%[0-9]+]] = objc_method [[SELF:%.*]] : $SwiftGizmo, #SwiftGizmo.x!getter.foreign : (SwiftGizmo) -> () -> X, $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
    // CHECK-NEXT: apply [[GETTER]]([[SELF]]) : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
    // CHECK-NOT: return
    // CHECK:   [[SETTER:%[0-9]+]] = objc_method [[SELF]] : $SwiftGizmo, #SwiftGizmo.x!setter.foreign : (SwiftGizmo) -> (X) -> (), $@convention(objc_method) (X, SwiftGizmo) -> ()
    // CHECK:  apply [[SETTER]]([[XMOD:%.*]], [[SELF]]) : $@convention(objc_method) (X, SwiftGizmo) -> ()
    x = x.foo()
    // CHECK: return
  }

  // CHECK-LABEL: sil hidden [ossa] @$s19objc_attr_NSManaged10SwiftGizmoC8testFunc{{[_0-9a-zA-Z]*}}F
  func testFunc() {
    // CHECK: = objc_method %0 : $SwiftGizmo, #SwiftGizmo.kvc!foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
    // CHECK: return
    kvc()
  }
}

extension SwiftGizmo {
  @NSManaged func extKVC()

  // CHECK-LABEL: $s19objc_attr_NSManaged10SwiftGizmoC7testExt{{[_0-9a-zA-Z]*}}F
  func testExt() {
    // CHECK: = objc_method %0 : $SwiftGizmo, #SwiftGizmo.extKVC!foreign : (SwiftGizmo) -> () -> (), $@convention(objc_method) (SwiftGizmo) -> ()
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

  // CHECK-LABEL: $s19objc_attr_NSManaged10FinalGizmoC8testExt2{{[_0-9a-zA-Z]*}}F
  func testExt2() {
    // CHECK: = objc_method %0 : $FinalGizmo, #FinalGizmo.extKVC2!foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
    // CHECK: return
    extKVC2()
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_attr_NSManaged9testFinalySSAA0E5GizmoCF : $@convention(thin) (@guaranteed FinalGizmo) -> @owned String {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $FinalGizmo):
// CHECK: objc_method [[ARG]] : $FinalGizmo, #FinalGizmo.kvc2!foreign : (FinalGizmo) -> () -> (), $@convention(objc_method) (FinalGizmo) -> ()
// CHECK-NOT: return
// CHECK: objc_method [[ARG]] : $FinalGizmo, #FinalGizmo.y!getter.foreign : (FinalGizmo) -> () -> String, $@convention(objc_method) (FinalGizmo) -> @autoreleased NSString
// CHECK: return
func testFinal(_ obj: FinalGizmo) -> String {
  obj.kvc2()
  return obj.y
}

// https://github.com/apple/swift/issues/45278
// '@NSManaged' property can't satisfy protocol requirement

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


// https://github.com/apple/swift/issues/49084
// '@NSManaged' properties can be 'final'

protocol EntityIDProto {
  var entityID: String { get set }
}

class FinalEntity: NSObject, EntityIDProto {
	@NSManaged final var entityID: String
}

// CHECK-LABEL: sil shared [ossa] @$s19objc_attr_NSManaged11FinalEntityC8entityIDSSvM : $@yield_once @convention(method) (@guaranteed FinalEntity) -> @yields @inout String
// CHECK: objc_method {{.*}} : $FinalEntity, #FinalEntity.entityID!getter.foreign
// CHECK: yield
// CHECK: objc_method {{.*}} : $FinalEntity, #FinalEntity.entityID!setter.foreign
// CHECK: return

// CHECK-NOT: sil hidden [ossa] @$s19objc_attr_NSManaged10SwiftGizmoC1xAA1XCfgTo : $@convention(objc_method) (SwiftGizmo) -> @autoreleased X
// CHECK-NOT: sil hidden [ossa] @$s19objc_attr_NSManaged10SwiftGizmoC1xAA1XCfsTo
// CHECK-NOT: sil hidden [ossa] @$s19objc_attr_NSManaged10{{[_0-9a-zA-Z]*}}FinalGizmoC1yytfgTo

// The vtable should not contain any entry points for getters and setters.
// CHECK-LABEL: sil_vtable SwiftGizmo {
// CHECK-NEXT:   #SwiftGizmo.modifyX: {{.*}} : @$s19objc_attr_NSManaged10SwiftGizmoC7modifyXyyF
// CHECK-NEXT:   #SwiftGizmo.testFunc: {{.*}} : @$s19objc_attr_NSManaged10SwiftGizmoC8testFuncyyF
// CHECK-NEXT:   #SwiftGizmo.deinit!deallocator: @$s19objc_attr_NSManaged10SwiftGizmoCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable FinalGizmo {
// CHECK-NEXT:   #SwiftGizmo.modifyX: {{.*}} : @$s19objc_attr_NSManaged10SwiftGizmoC7modifyX{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #SwiftGizmo.testFunc: {{.*}} : @$s19objc_attr_NSManaged10SwiftGizmoC8testFunc{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT:   #FinalGizmo.deinit!deallocator: @$s19objc_attr_NSManaged10FinalGizmoCfD
// CHECK-NEXT: }

// CHECK-LABEL: sil_vtable ProtoAdopter {
// CHECK-NOT: managed{{.*}}Prop
// CHECK: {{^}$}}
