// RUN: %swift %s -emit-silgen -emit-verbose-sil | FileCheck %s

class A {
  var [objc] prop : Int
  var [objc] computedProp : Int {
  get:
    return 5

  set:
  }
}

// CHECK-LABEL: sil @_T15objc_properties11testPropGetFT1aCS_1A_Si : $[thin] (a : A) -> Int64
func testPropGet(a : A) -> Int {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.prop!getter.1.foreign : $[cc(objc_method), thin] ((), A) -> Int64
  return a.prop
}

// CHECK-LABEL: sil @_T15objc_properties11testPropSetFT1aCS_1A1iSi_T_ : $[thin] (a : A, i : Int64) -> ()
func testPropSet(a : A, i : Int) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.prop!setter.1.foreign : $[cc(objc_method), thin] ((value : Int64), A) -> ()
  a.prop = i
}

// CHECK-LABEL: sil @_T15objc_properties19testComputedPropGetFT1aCS_1A_Si : $[thin] (a : A) -> Int64
func testComputedPropGet(a : A) -> Int {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.computedProp!getter.1.foreign : $[cc(objc_method), thin] ((), A) -> Int64
  return a.computedProp
}

// CHECK-LABEL: sil @_T15objc_properties19testComputedPropSetFT1aCS_1A1iSi_T_ : $[thin] (a : A, i : Int64) -> ()
func testComputedPropSet(a : A, i : Int) {
  // CHECK: class_method [volatile] [[OBJ:%[0-9]+]] : $A, #A.computedProp!setter.1.foreign : $[cc(objc_method), thin] ((value : Int64), A) -> ()
  a.computedProp = i
}
