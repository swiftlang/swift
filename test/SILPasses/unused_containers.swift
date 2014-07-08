// RUN: %swift %s -O3 -emit-sil | FileCheck %s

//CHECK-LABEL: @_TF17unused_containers16empty_array_testFT_T_
//CHECK-NEXT: bb0:
//CHECK-NEXT: tuple
//CHECK-NEXT: return
func empty_array_test() {
  let unused : [Int] = []
}

//CHECK-LABEL: @_TF17unused_containers14empty_dic_testFT_T_
//CHECK-NEXT: bb0:
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
