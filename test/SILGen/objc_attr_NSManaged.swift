// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs %s -emit-silgen | FileCheck %s

import gizmo

@objc class X { 
  func foo() -> X { return self }
}

class SwiftGizmo : Gizmo {
  @NSManaged var x: X

  // CHECK-NOT: sil @_TToFC19objc_attr_NSManaged10SwiftGizmog1xCS_1X : $@cc(objc_method) @thin (SwiftGizmo) -> @autoreleased X
  // CHECK-NOT: sil @_TToFC19objc_attr_NSManaged10SwiftGizmos1xCS_1X

  // Make sure that we're calling through the @objc entry points.
  // CHECK-LABEL: sil @_TFC19objc_attr_NSManaged10SwiftGizmo7modifyX{{.*}} : $@cc(method) @thin (@owned SwiftGizmo) -> () {
  func modifyX() {
    // CHECK:   [[GETTER:%[0-9]+]] = class_method [volatile] [[SELF:%.*]] : $SwiftGizmo, #SwiftGizmo.x!getter.1.foreign : SwiftGizmo -> () -> X , $@cc(objc_method) @thin (SwiftGizmo) -> @autoreleased X
    // CHECK-NEXT: apply [[GETTER]]([[SELF]]) : $@cc(objc_method) @thin (SwiftGizmo) -> @autoreleased X
    // CHECK-NOT: return
    // CHECK:   [[SETTER:%[0-9]+]] = class_method [volatile] [[SELF]] : $SwiftGizmo, #SwiftGizmo.x!setter.1.foreign : SwiftGizmo -> (value: X) -> () , $@cc(objc_method) @thin (X, SwiftGizmo) -> ()
    // CHECK:  apply [[SETTER]]([[XMOD:%.*]], [[SELF]]) : $@cc(objc_method) @thin (X, SwiftGizmo) -> ()
    x = x.foo()
    // CHECK: return
  }
}

// CHECK-NOT: sil @_TToFC19objc_attr_NSManaged10SwiftGizmog1xCS_1X : $@cc(objc_method) @thin (SwiftGizmo) -> @autoreleased X
// CHECK-NOT: sil @_TToFC19objc_attr_NSManaged10SwiftGizmos1xCS_1X
