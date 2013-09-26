// RUN: %swift %s -emit-silgen -emit-verbose-sil | FileCheck %s

class A {
  subscript [objc] (i : Int) -> A {
  get:
    return self

  set:
  }
}

// CHECK-LABEL: sil @_T14objc_subscript16testSubscriptGetFT1aCS_1A1iSi_S0_ : $[thin] (a : A, i : Int64) -> A
func testSubscriptGet(a : A, i : Int) -> A {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.__subscript!getter.2.foreign : $[cc(objc_method), thin] ((), (i : Int64), A) -> A
  return a[i]
}

// CHECK-LABEL: sil @_T14objc_subscript16testSubscriptSetFT1aCS_1A1iSi_T_ : $[thin] (a : A, i : Int64) -> ()
func testSubscriptSet(a : A, i : Int) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.__subscript!setter.2.foreign : $[cc(objc_method), thin] ((value : A), (i : Int64), A) -> ()
  a[i] = a
}
