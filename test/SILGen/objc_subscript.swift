// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-silgen -emit-verbose-sil -disable-objc-attr-requires-foundation-module | %FileCheck %s

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

// CHECK-LABEL: sil hidden @_T014objc_subscript16testSubscriptGet{{[_0-9a-zA-Z]*}}F
func testSubscriptGet(a: A, i: Int) -> ObjCClass {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.subscript!getter.1.foreign : (A) -> (Int) -> ObjCClass, $@convention(objc_method) (Int, A) -> @autoreleased ObjCClass
  return a[i]
}

// CHECK-LABEL: sil hidden @_T014objc_subscript16testSubscriptSet{{[_0-9a-zA-Z]*}}F
func testSubscriptSet(a: A, i: Int, v: ObjCClass) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.subscript!setter.1.foreign : (A) -> (ObjCClass, Int) -> (), $@convention(objc_method) (ObjCClass, Int, A) -> ()
  a[i] = v
}

// 'super' subscript usage
class B : A {
  @objc override subscript (i: Int) -> ObjCClass {
    // CHECK-LABEL: sil hidden @_T014objc_subscript1BC0B0AA9ObjCClassCSicfg : $@convention(method) (Int, @guaranteed B) -> @owned ObjCClass
    get {
      // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $B, #A.subscript!getter.1.foreign : (A) -> (Int) -> ObjCClass, $@convention(objc_method) (Int, A) -> @autoreleased ObjCClass
      return super[i]
    }
    // CHECK-LABEL: sil hidden @_T014objc_subscript1BC0B0AA9ObjCClassCSicfs : $@convention(method) (@owned ObjCClass, Int, @guaranteed B) -> ()
    set(value) {
      // CHECK: super_method [volatile] [[SELF:%[0-9]+]] : $B, #A.subscript!setter.1.foreign : (A) -> (ObjCClass, Int) -> (), $@convention(objc_method) (ObjCClass, Int, A) -> ()
      super[i] = value
    }
  }
}
