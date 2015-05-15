// RUN: %target-swift-frontend -primary-file %s -O -emit-sil | FileCheck %s

//CHECK-LABEL: @_TF17unused_containers16empty_array_testFT_T_
//CHECK: bb0:
//CHECK-NEXT: tuple
//CHECK-NEXT: return
func empty_array_test() {
  let unused : [Int] = []
}

//CHECK-LABEL: @_TF17unused_containers14empty_dic_testFT_T_
//CHECK: bb0:
//CHECK-NEXT: tuple
//CHECK-NEXT: return
func empty_dic_test() {
  let unused : [Int:Int] = [:]
}

//CHECK-LABEL: @_TF17unused_containers18unused_string_testFT_T_
//CHECK-NEXT: bb0:
//CHECK-NEXT: tuple
//CHECK-NEXT: return
func unused_string_test() {
  let unused : String = ""
}

// FIXME: <rdar://problem/20980377> Add dead array elimination to DeadObjectElimination
//CHECK-LABEL: array_of_strings_test
//CHECK: bb0:
//DISABLED-CHECK-NEXT: tuple
//DISABLED-CHECK-NEXT: return
func array_of_strings_test() {
  let x = [""]
}

// FIXME: <rdar://problem/20980377> Add dead array elimination to DeadObjectElimination
//CHECK-LABEL: string_interpolation
//CHECK: bb0:
//DISABLED-CHECK-NEXT: tuple
//DISABLED-CHECK-NEXT: return
func string_interpolation() {
  // Int
  let x : Int = 2
  "\(x)"

  // String
  let y : String = "hi"
  "\(y)"

  // Float
  let f : Float = 2.0
  "\(f)"

  // Bool
  "\(true)"

  //UInt8
  "\(UInt8(2))"

  //UInt32
  "\(UInt32(4))"
}

// FIXME: <rdar://problem/20980377> Add dead array elimination to DeadObjectElimination
//CHECK-LABEL: string_interpolation2
//CHECK: bb0:
//DISABLED-CHECK-NEXT: tuple
//DISABLED-CHECK-NEXT: return
func string_interpolation2() {
  "\(false) \(true)"
}

//CHECK-LABEL: string_plus
//CHECK: bb0:
//CHECK-NEXT: tuple
//CHECK-NEXT: return
func string_plus() {
  "a" + "b"
}
