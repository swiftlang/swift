// RUN: %target-swift-frontend %s -emit-sil -O \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Slab \
// RUN:   -enable-experimental-feature ValueGenerics | %FileCheck %s --check-prefix=CHECK-SIL 

// RUN: %target-swift-frontend %s -emit-ir -O \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Slab \
// RUN:   -enable-experimental-feature ValueGenerics | %FileCheck %s  --check-prefix=CHECK-IR 

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_ValueGenerics
// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// Bounds check should be eliminated
// SIL removes lower bounds check from the loop
// LLVM removes the upper bounds check from the loop and then vectorizes
// A lower bounds check is left behind in the entry block

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A29_sum_iterate_to_count_wo_trapySis4SlabVyxSiGSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A29_sum_iterate_to_count_wo_trapySis4SlabVyxSiGSiRVzlF'

// CHECK-IR: define {{.*}} @"$s23slab_bounds_check_tests0A29_sum_iterate_to_count_wo_trapySis4SlabVyxSiGSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func slab_sum_iterate_to_count_wo_trap<let N: Int>(_ v: Slab<N, Int>) -> Int {
  var sum = 0
  for i in 0..<v.count {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be eliminated
// SIL removes lower bounds check from the loop
// LLVM removes the upper bounds check from the loop
// A lower bounds check is left behind in the entry block

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A31_sum_iterate_to_count_with_trapySis4SlabVyxSiGSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A31_sum_iterate_to_count_with_trapySis4SlabVyxSiGSiRVzlF'

public func slab_sum_iterate_to_count_with_trap<let N: Int>(_ v: Slab<N, Int>) -> Int {
  var sum = 0
  for i in 0..<v.count {
    sum += v[i]
  }
  return sum
}

// Bounds check should be hoisted
// SIL removes lower bounds check from the loop
// LLVM removes the upper bounds check from the loop and then vectorizes
// A lower bounds check is left behind in the entry block

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A31_sum_iterate_to_unknown_wo_trapySis4SlabVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A31_sum_iterate_to_unknown_wo_trapySis4SlabVyxSiG_SitSiRVzlF'

// CHECK-IR: define {{.*}} @"$s23slab_bounds_check_tests0A31_sum_iterate_to_unknown_wo_trapySis4SlabVyxSiG_SitSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func slab_sum_iterate_to_unknown_wo_trap<let N: Int>(_ v: Slab<N, Int>, _ n: Int) -> Int {
  var sum = 0
  for i in 0...n {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be hoisted
// SIL removes lower bounds check from the loop
// LLVM removes the upper bounds check from the loop
// A lower bounds check is left behind in the entry block

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A33_sum_iterate_to_unknown_with_trapySis4SlabVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A33_sum_iterate_to_unknown_with_trapySis4SlabVyxSiG_SitSiRVzlF'
public func slab_sum_iterate_to_unknown_with_trap<let N: Int>(_ v: Slab<N, Int>, _ n: Int) -> Int {
  var sum = 0
  for i in 0...n {
    sum += v[i]
  }
  return sum
}

// Bounds check should be eliminated
// SIL removes lower bounds check from the loop
// LLVM removes the upper bounds check from the loop and then vectorizes

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A40_sum_iterate_to_deducible_count1_wo_trapySis4SlabVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A40_sum_iterate_to_deducible_count1_wo_trapySis4SlabVyxSiG_SitSiRVzlF'

// CHECK-IR: define {{.*}} @"$s23slab_bounds_check_tests0A40_sum_iterate_to_deducible_count1_wo_trapySis4SlabVyxSiG_SitSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func slab_sum_iterate_to_deducible_count1_wo_trap<let N: Int>(_ v: Slab<N, Int>, _ n: Int) -> Int {
  var sum = 0
  precondition(n <= v.count)
  for i in 0..<n {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be eliminated
// SIL removes lower bounds check from the loop
// LLVM does not eliminate redundant bounds check 

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A42_sum_iterate_to_deducible_count1_with_trapySis4SlabVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A42_sum_iterate_to_deducible_count1_with_trapySis4SlabVyxSiG_SitSiRVzlF'
public func slab_sum_iterate_to_deducible_count1_with_trap<let N: Int>(_ v: Slab<N, Int>, _ n: Int) -> Int {
  var sum = 0
  precondition(n <= v.count)
  for i in 0..<n {
    sum += v[i]
  }
  return sum
}

// Bounds check should be eliminated
// SIL removes lower bounds check from the loop
// LLVM removes upper bounds check and vectorizes the loop 

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A40_sum_iterate_to_deducible_count2_wo_trapySis4SlabVyxSiG_SitSiRVzlF :
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A40_sum_iterate_to_deducible_count2_wo_trapySis4SlabVyxSiG_SitSiRVzlF'

// CHECK-IR: define {{.*}} @"$s23slab_bounds_check_tests0A40_sum_iterate_to_deducible_count2_wo_trapySis4SlabVyxSiG_SitSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func slab_sum_iterate_to_deducible_count2_wo_trap<let N: Int>(_ v: Slab<N, Int>, _ n: Int) -> Int {
  var sum = 0
  precondition(n <= v.count)
  for i in 0...n {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be eliminated
// SIL removes lower bounds check from the loop
// LLVM does not eliminate redundant bounds check 

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A42_sum_iterate_to_deducible_count2_with_trapySis4SlabVyxSiG_SitSiRVzlF : 
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A42_sum_iterate_to_deducible_count2_with_trapySis4SlabVyxSiG_SitSiRVzlF'
public func slab_sum_iterate_to_deducible_count2_with_trap<let N: Int>(_ v: Slab<N, Int>, _ n: Int) -> Int {
  var sum = 0
  precondition(n <= v.count)
  for i in 0...n {
    sum += v[i]
  }
  return sum
}

// Bounds check should be eliminated
// SIL removes lower bounds check from the loop
// LLVM removes upper bounds check and vectorizes the loop 

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A29_iterate_over_indices_wo_trapySis4SlabVyxSiGSiRVzlF : 
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A29_iterate_over_indices_wo_trapySis4SlabVyxSiGSiRVzlF'

// CHECK-IR: define {{.*}} @"$s23slab_bounds_check_tests0A29_iterate_over_indices_wo_trapySis4SlabVyxSiGSiRVzlF"
// CHECK-IR: @llvm.vector.reduce.add
public func slab_iterate_over_indices_wo_trap<let N: Int>(_ v: Slab<N, Int>) -> Int {
  var sum = 0
  for i in v.indices {
    sum &+= v[i]
  }
  return sum
}

// Bounds check should be eliminated
// SIL removes lower bounds check from the loop
// LLVM does not eliminate redundant bounds check 

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A31_iterate_over_indices_with_trapySis4SlabVyxSiGSiRVzlF : 
// CHECK-SIL: bb3
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A31_iterate_over_indices_with_trapySis4SlabVyxSiGSiRVzlF'
public func slab_iterate_over_indices_with_trap<let N: Int>(_ v: Slab<N, Int>) -> Int {
  var sum = 0
  for i in v.indices {
    sum += v[i]
  }
  return sum
}

// Eliminate duplicate bounds check

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A17_element_equalityySbs4SlabVyxSiG_SitSiRVzlF : 
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A17_element_equalityySbs4SlabVyxSiG_SitSiRVzlF'

public func slab_element_equality<let N: Int>(_ v: Slab<N, Int>, _ i: Int) -> Bool {
  return v[i] == v[i]
}

// Eliminate duplicate bounds check

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A12_element_sumySis4SlabVyxSiG_SitSiRVzlF :
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A12_element_sumySis4SlabVyxSiG_SitSiRVzlF'
public func slab_element_sum<let N: Int>(_ v: Slab<N, Int>, _ i: Int) -> Int {
  return v[i] &+ v[i]
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A7_searchySiSgs4SlabVyq_xG_xtSiRV_SQRzr0_lF : 
// CHECK-SIL: bb3:
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A7_searchySiSgs4SlabVyq_xG_xtSiRV_SQRzr0_lF'
public func slab_search<T : Equatable, let N: Int>(_ v: Slab<N, T>, _ elem: T) -> Int? {
  for i in v.indices {
    if v[i] == elem {
      return i
    }
  }
  return nil
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A11_search_splySiSgs4SlabVyxSiG_SitSiRVzlF : 
// CHECK-SIL: bb3:
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A11_search_splySiSgs4SlabVyxSiG_SitSiRVzlF'
public func slab_search_spl<let N: Int>(_ v: Slab<N, Int>, _ elem: Int) -> Int? {
  for i in v.indices {
    if v[i] == elem {
      return i
    }
  }
  return nil
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s23slab_bounds_check_tests0A18_binary_search_splySiSgs4SlabVyxSiG_SitSiRVzlF : 
// CHECK-SIL: bb2
// CHECK-SIL: cond_fail {{.*}}, "Index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s23slab_bounds_check_tests0A18_binary_search_splySiSgs4SlabVyxSiG_SitSiRVzlF'
public func slab_binary_search_spl<let N: Int>(_ v: Slab<N, Int>, _ elem: Int) -> Int? {
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

