// RUN: %target-swift-frontend %s -O -module-name=test -emit-sil | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

var gg = 0

@inline(never)
func take(_ x: Int, _ y: Int) {
  gg = x + y
}

// CHECK-LABEL: sil @$s4test23check_cond_fail_messageySiSaySiGF
// CHECK:  cond_fail {{.*}} "Index out of range"
// CHECK: // end sil function '$s4test23check_cond_fail_messageySiSaySiGF'
public func check_cond_fail_message(_ array: [Int]) -> Int {
  return array[2]
}

// CHECK-LABEL: sil @$s4test22eliminate_bounds_checkyySaySiGF
// CHECK-NOT:  cond_fail {{.*}} "Index out of range"
// CHECK: // end sil function '$s4test22eliminate_bounds_checkyySaySiGF'
public func eliminate_bounds_check(_ array: [Int]) {
  for (index, x) in array.enumerated() {
      take(x, index)
  }
}

// CHECK-LABEL: sil @$s4test27eliminate_two_bounds_checksyySaySiGF
// CHECK-NOT:  cond_fail {{.*}} "Index out of range"
// CHECK: // end sil function '$s4test27eliminate_two_bounds_checksyySaySiGF'
public func eliminate_two_bounds_checks(_ array: [Int]) {
  for (index, x) in array.enumerated() {
      take(x, array[index])
  }
}

