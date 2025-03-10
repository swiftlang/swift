// RUN: %target-swift-frontend %s -emit-sil -O \
// RUN:   -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-SIL 

// RUN: %target-swift-frontend %s -emit-ir -O \
// RUN:   -disable-availability-checking | %FileCheck %s  --check-prefix=CHECK-IR 

// REQUIRES: swift_in_compiler
// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// Bounds check should be eliminated
// Induction variable optimization eliminates the bounds check in SIL
// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A29_sum_iterate_to_count_wo_trapySis11InlineArrayVyxSiGSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A29_sum_iterate_to_count_wo_trapySis11InlineArrayVyxSiGSiRVzlF'

// CHECK-IR: define {{.*}} @"$s30inlinearray_bounds_check_tests0A29_sum_iterate_to_count_wo_trapySis11InlineArrayVyxSiGSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func inlinearray_sum_iterate_to_count_wo_trap<let N: Int>(_ v: InlineArray<N, Int>) -> Int {
  var sum = 0
  for i in 0..<v.count {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be eliminated
// Induction variable optimization eliminates the bounds check in SIL

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A31_sum_iterate_to_count_with_trapySis11InlineArrayVyxSiGSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A31_sum_iterate_to_count_with_trapySis11InlineArrayVyxSiGSiRVzlF'

public func inlinearray_sum_iterate_to_count_with_trap<let N: Int>(_ v: InlineArray<N, Int>) -> Int {
  var sum = 0
  for i in 0..<v.count {
    sum += v[i]
  }
  return sum
}

// Bounds check should be hoisted

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A31_sum_iterate_to_unknown_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A31_sum_iterate_to_unknown_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF'

// CHECK-IR: define {{.*}} @"$s30inlinearray_bounds_check_tests0A31_sum_iterate_to_unknown_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func inlinearray_sum_iterate_to_unknown_wo_trap<let N: Int>(_ v: InlineArray<N, Int>, _ n: Int) -> Int {
  var sum = 0
  for i in 0...n {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be hoisted

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A33_sum_iterate_to_unknown_with_trapySis11InlineArrayVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A33_sum_iterate_to_unknown_with_trapySis11InlineArrayVyxSiG_SitSiRVzlF'
public func inlinearray_sum_iterate_to_unknown_with_trap<let N: Int>(_ v: InlineArray<N, Int>, _ n: Int) -> Int {
  var sum = 0
  for i in 0...n {
    sum += v[i]
  }
  return sum
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A40_sum_iterate_to_deducible_count1_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A40_sum_iterate_to_deducible_count1_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF'

// CHECK-IR: define {{.*}} @"$s30inlinearray_bounds_check_tests0A40_sum_iterate_to_deducible_count1_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func inlinearray_sum_iterate_to_deducible_count1_wo_trap<let N: Int>(_ v: InlineArray<N, Int>, _ n: Int) -> Int {
  var sum = 0
  precondition(n <= v.count)
  for i in 0..<n {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A42_sum_iterate_to_deducible_count1_with_trapySis11InlineArrayVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A42_sum_iterate_to_deducible_count1_with_trapySis11InlineArrayVyxSiG_SitSiRVzlF'
public func inlinearray_sum_iterate_to_deducible_count1_with_trap<let N: Int>(_ v: InlineArray<N, Int>, _ n: Int) -> Int {
  var sum = 0
  precondition(n <= v.count)
  for i in 0..<n {
    sum += v[i]
  }
  return sum
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A40_sum_iterate_to_deducible_count2_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A40_sum_iterate_to_deducible_count2_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF'

// CHECK-IR: define {{.*}} @"$s30inlinearray_bounds_check_tests0A40_sum_iterate_to_deducible_count2_wo_trapySis11InlineArrayVyxSiG_SitSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func inlinearray_sum_iterate_to_deducible_count2_wo_trap<let N: Int>(_ v: InlineArray<N, Int>, _ n: Int) -> Int {
  var sum = 0
  precondition(n <= v.count)
  for i in 0...n {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A42_sum_iterate_to_deducible_count2_with_trapySis11InlineArrayVyxSiG_SitSiRVzlF : 
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A42_sum_iterate_to_deducible_count2_with_trapySis11InlineArrayVyxSiG_SitSiRVzlF'
public func inlinearray_sum_iterate_to_deducible_count2_with_trap<let N: Int>(_ v: InlineArray<N, Int>, _ n: Int) -> Int {
  var sum = 0
  precondition(n <= v.count)
  for i in 0...n {
    sum += v[i]
  }
  return sum
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A29_iterate_over_indices_wo_trapySis11InlineArrayVyxSiGSiRVzlF : 
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A29_iterate_over_indices_wo_trapySis11InlineArrayVyxSiGSiRVzlF'

// CHECK-IR: define {{.*}} @"$s30inlinearray_bounds_check_tests0A29_iterate_over_indices_wo_trapySis11InlineArrayVyxSiGSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func inlinearray_iterate_over_indices_wo_trap<let N: Int>(_ v: InlineArray<N, Int>) -> Int {
  var sum = 0
  for i in v.indices {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be eliminated
// Induction variable optimization eliminates the bounds check in SIL

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A31_iterate_over_indices_with_trapySis11InlineArrayVyxSiGSiRVzlF : 
// CHECK-SIL: bb3
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A31_iterate_over_indices_with_trapySis11InlineArrayVyxSiGSiRVzlF'
public func inlinearray_iterate_over_indices_with_trap<let N: Int>(_ v: InlineArray<N, Int>) -> Int {
  var sum = 0
  for i in v.indices {
    sum += v[i]
  }
  return sum
}

// Eliminate duplicate bounds check

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A17_element_equalityySbs11InlineArrayVyxSiG_SitSiRVzlF : 
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A17_element_equalityySbs11InlineArrayVyxSiG_SitSiRVzlF'
public func inlinearray_element_equality<let N: Int>(_ v: InlineArray<N, Int>, _ i: Int) -> Bool {
  return v[i] == v[i]
}

// Eliminate duplicate bounds check

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A12_element_sumySis11InlineArrayVyxSiG_SitSiRVzlF :
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A12_element_sumySis11InlineArrayVyxSiG_SitSiRVzlF'
public func inlinearray_element_sum<let N: Int>(_ v: InlineArray<N, Int>, _ i: Int) -> Int {
  return v[i] &+ v[i]
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A7_searchySiSgs11InlineArrayVyq_xG_xtSiRV_SQRzr0_lF : 
// CHECK-SIL: bb3:
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A7_searchySiSgs11InlineArrayVyq_xG_xtSiRV_SQRzr0_lF'
public func inlinearray_search<T : Equatable, let N: Int>(_ v: InlineArray<N, T>, _ elem: T) -> Int? {
  for i in v.indices {
    if v[i] == elem {
      return i
    }
  }
  return nil
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A11_search_splySiSgs11InlineArrayVyxSiG_SitSiRVzlF : 
// CHECK-SIL: bb3:
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A11_search_splySiSgs11InlineArrayVyxSiG_SitSiRVzlF'
public func inlinearray_search_spl<let N: Int>(_ v: InlineArray<N, Int>, _ elem: Int) -> Int? {
  for i in v.indices {
    if v[i] == elem {
      return i
    }
  }
  return nil
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A18_binary_search_splySiSgs11InlineArrayVyxSiG_SitSiRVzlF : 
// CHECK-SIL: bb2
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A18_binary_search_splySiSgs11InlineArrayVyxSiG_SitSiRVzlF'
public func inlinearray_binary_search_spl<let N: Int>(_ v: InlineArray<N, Int>, _ elem: Int) -> Int? {
  var low = 0, high = v.count - 1
  while low <= high {
    let mid = low + (high - low) / 2

    if v[mid] == elem {
      return mid
    }
    else if v[mid] < elem {
      low = mid + 1
    } else {
      high = mid - 1
    }
  }

  return nil;
}

// InlineArray is copied into a temporary within the loop in the "specialized" version
// This prevents LoopRotate which prevent bounds checks opts since it depends on induction variable analysis which doesn't work on unrotated loops.
// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A35_sum_iterate_to_count_with_trap_splySis11InlineArrayVy$63_SiGF :
// CHECK-SIL: bb2
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A35_sum_iterate_to_count_with_trap_splySis11InlineArrayVy$63_SiGF'
public func inlinearray_sum_iterate_to_count_with_trap_spl(_ v: InlineArray<64, Int>) -> Int {
  var sum = 0
  for i in v.indices {
    sum &+= v[i]
  }
  return sum
}

// InlineArray is copied into a temporary within the loop in the "specialized" version
// This prevents LoopRotate which prevent bounds checks opts since it depends on induction variable analysis which doesn't work on unrotated loops. 
// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A37_sum_iterate_to_unknown_with_trap_splySis11InlineArrayVy$63_SiG_SitF :
// CHECK-SIL: bb2
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A37_sum_iterate_to_unknown_with_trap_splySis11InlineArrayVy$63_SiG_SitF'
public func inlinearray_sum_iterate_to_unknown_with_trap_spl(_ v: InlineArray<64, Int>, _ n: Int) -> Int {
  var sum = 0
  for i in 0...n {
    sum &+= v[i]
  }
  return sum
}

// Current codegen for this in SIL is very poor
// First a temp is created and the elements are stored to it, then they get loaded from the temp to be stored in the let which is then loaded again to get the sum
// However, LLVM can constant fold everything 
public func local_inlinearray_sum_iterate_to_count_with_trap_spl() -> Int {
  var sum = 0
  let v : InlineArray = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16]
  for i in v.indices {
    sum += v[i]
  }
  return sum
}

// Current codegen for this in SIL is very poor
// First a temp is created and the elements are stored to it, then they get loaded from the temp to be stored in the let which is then loaded again to get the sum
// LLVM cannot constant fold, it memsets, memcopies and then loops over to sum  
public func local_inlinearray_repeating_init_sum_iterate_to_count_trap_spl() -> Int {
  var sum = 0
  let v = InlineArray<64, Int>(repeating: 64)

  for i in v.indices {
    sum += v[i]
  }

  return sum
}

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A11_inc_by_oneyys11InlineArrayVyxSiGz_SitSiRVzlF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
public func inlinearray_inc_by_one<let N: Int>(_ v: inout InlineArray<N, Int>, _ n: Int) {
  for i in v.indices {
    v[i] += 1
  }
}

// CHECK-SIL-LABEL: sil @$s30inlinearray_bounds_check_tests0A15_inc_by_one_splyys11InlineArrayVy$63_SiGz_SitF : 
// CHECK-SIL: bb2
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s30inlinearray_bounds_check_tests0A15_inc_by_one_splyys11InlineArrayVy$63_SiGz_SitF'
public func inlinearray_inc_by_one_spl(_ v: inout InlineArray<64, Int>, _ n: Int) {
  for i in v.indices {
    v[i] += 1
  }
}
