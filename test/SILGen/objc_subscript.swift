// RUN: %target-swift-frontend %s -emit-silgen -emit-verbose-sil -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: objc_interop

@objc class ObjCClass {}

class A {
  dynamic subscript (i: Int) -> ObjCClass {
    get {
      return ObjCClass()
    }
    set {}
  }
}

// CHECK-LABEL: sil hidden @_TF14objc_subscript16testSubscriptGet
func testSubscriptGet(a: A, i: Int) -> ObjCClass {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.subscript!getter.1.foreign : A -> (Int) -> ObjCClass , $@cc(objc_method) @thin (Int, A) -> @autoreleased ObjCClass
  return a[i]
}

// CHECK-LABEL: sil hidden @_TF14objc_subscript16testSubscriptSet
func testSubscriptSet(a: A, i: Int, v: ObjCClass) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.subscript!setter.1.foreign : A -> (ObjCClass, Int) -> () , $@cc(objc_method) @thin (ObjCClass, Int, A) -> ()
  a[i] = v
}

// 'super' subscript usage
class B : A {
  @objc override subscript (i: Int) -> ObjCClass {
    // CHECK-LABEL: sil hidden @_TFC14objc_subscript1Bg9subscriptFSiCS_9ObjCClass : $@cc(method) @thin (Int, @guaranteed B) -> @owned ObjCClass
    get {
      // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $B, #A.subscript!getter.1.foreign : A -> (Int) -> ObjCClass , $@cc(objc_method) @thin (Int, A) -> @autoreleased ObjCClass
      return super[i]
    }
    // CHECK-LABEL: sil hidden @_TFC14objc_subscript1Bs9subscriptFSiCS_9ObjCClass : $@cc(method) @thin (@owned ObjCClass, Int, @guaranteed B) -> ()
    set(value) {
      // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $B, #A.subscript!setter.1.foreign : A -> (ObjCClass, Int) -> () , $@cc(objc_method) @thin (ObjCClass, Int, A) -> ()
      super[i] = value
    }
  }
}
