// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -sil-print-types %s -emit-verbose-sil -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

@objc class ObjCClass {}

class A {
  @objc dynamic subscript (i: Int) -> ObjCClass {
    get {
      return ObjCClass()
    }
    set {}
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s14objc_subscript16testSubscriptGet{{[_0-9a-zA-Z]*}}F
func testSubscriptGet(a: A, i: Int) -> ObjCClass {
  // CHECK: objc_method [[OBJ:%[0-9]+]] : $A, #A.subscript!getter.foreign : (A) -> (Int) -> ObjCClass, $@convention(objc_method) (Int, A) -> @autoreleased ObjCClass
  return a[i]
}

// CHECK-LABEL: sil hidden [ossa] @$s14objc_subscript16testSubscriptSet{{[_0-9a-zA-Z]*}}F
func testSubscriptSet(a: A, i: Int, v: ObjCClass) {
  // CHECK: objc_method [[OBJ:%[0-9]+]] : $A, #A.subscript!setter.foreign : (A) -> (ObjCClass, Int) -> (), $@convention(objc_method) (ObjCClass, Int, A) -> ()
  a[i] = v
}

// 'super' subscript usage
class B : A {
  @objc override subscript (i: Int) -> ObjCClass {
    // CHECK-LABEL: sil hidden [ossa] @$s14objc_subscript1BCyAA9ObjCClassCSicig : $@convention(method) (Int, @guaranteed B) -> @owned ObjCClass
    get {
      // CHECK: objc_super_method [[SELF:%[0-9]+]] : $B, #A.subscript!getter.foreign : (A) -> (Int) -> ObjCClass, $@convention(objc_method) (Int, A) -> @autoreleased ObjCClass
      return super[i]
    }
    // CHECK-LABEL: sil hidden [ossa] @$s14objc_subscript1BCyAA9ObjCClassCSicis : $@convention(method) (@owned ObjCClass, Int, @guaranteed B) -> ()
    set(value) {
      // CHECK: objc_super_method [[SELF:%[0-9]+]] : $B, #A.subscript!setter.foreign : (A) -> (ObjCClass, Int) -> (), $@convention(objc_method) (ObjCClass, Int, A) -> ()
      super[i] = value
    }
  }
}

protocol SubscriptProto {
  subscript(i: Int) -> Any! { get }
}
extension Guisemeau: SubscriptProto {}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$sSo9GuisemeauC14objc_subscript14SubscriptProtoA2cDPyypSgSicigTW
// CHECK: function_ref @$sSo9GuisemeauCyypSgSicigTO
// CHECK: end sil function '$sSo9GuisemeauC14objc_subscript14SubscriptProtoA2cDPyypSgSicigTW'

// CHECK-LABEL: sil shared [serialized] [thunk] [ossa] @$sSo9GuisemeauCyypSgSicigTO
// CHECK: objc_method {{%[0-9]+}} : $Guisemeau, #Guisemeau.subscript!getter.foreign : (Guisemeau) -> (Int) -> Any?, $@convention(objc_method) (Int, Guisemeau) -> @autoreleased Optional<AnyObject>
// CHECK: end sil function '$sSo9GuisemeauCyypSgSicigTO'
