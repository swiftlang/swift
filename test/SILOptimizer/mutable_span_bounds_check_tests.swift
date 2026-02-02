// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I %t -O -emit-sil %s -enable-experimental-feature Lifetimes -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-SIL
// RUN: %target-swift-frontend -I %t -O -emit-ir %s -enable-experimental-feature Lifetimes -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-IR

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0a1_B11_sum_w_trapySis11MutableSpanVySiGF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0a1_B11_sum_w_trapySis11MutableSpanVySiGF'
public func mutable_span_sum_w_trap(_ ms: borrowing MutableSpan<Int>) -> Int {
  var sum = 0
  for i in ms.indices {
    sum += ms[i]
  }
  return sum
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0a1_B12_sum_wo_trapySis11MutableSpanVySiGF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0a1_B12_sum_wo_trapySis11MutableSpanVySiGF'
public func mutable_span_sum_wo_trap(_ ms: borrowing MutableSpan<Int>) -> Int {
  var sum = 0
  for i in ms.indices {
    sum &+= ms[i]
  }
  return sum
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0a1_B25_sum_w_trap_unknown_boundySis11MutableSpanVySiG_SitF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0a1_B25_sum_w_trap_unknown_boundySis11MutableSpanVySiG_SitF'
public func mutable_span_sum_w_trap_unknown_bound(_ ms: borrowing MutableSpan<Int>, _ n: Int) -> Int {
  var sum = 0
  for i in 0...n {
    sum += ms[i]
  }
  return sum
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0a1_B26_sum_wo_trap_unknown_boundySis11MutableSpanVySiG_SitF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0a1_B26_sum_wo_trap_unknown_boundySis11MutableSpanVySiG_SitF'
public func mutable_span_sum_wo_trap_unknown_bound(_ ms: borrowing MutableSpan<Int>, _ n: Int) -> Int {
  var sum = 0
  for i in 0...n {
    sum &+= ms[i]
  }
  return sum
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B10_zero_inityys11MutableSpanVySiGzF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B10_zero_inityys11MutableSpanVySiGzF'

// CHECK-IR: define {{.*}} void @"$s31mutable_span_bounds_check_tests0B10_zero_inityys11MutableSpanVySiGzF"
// CHECK-IR: call void @llvm.memset
public func span_zero_init(_ output: inout MutableSpan<Int>) {
  for i in output.indices {
    output[i] = 0
  }
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B14_copy_elemwiseyys11MutableSpanVySiGz_s0I0VySiGtF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B14_copy_elemwiseyys11MutableSpanVySiGz_s0I0VySiGtF'

// CHECK-IR: define {{.*}} void @"$s31mutable_span_bounds_check_tests0B14_copy_elemwiseyys11MutableSpanVySiGz_s0I0VySiGtF"
// CHECK-IR: vector.body
// CHECK-IR: store <{{.*}}>
@_lifetime(output: copy output, copy input)
public func span_copy_elemwise(_ output: inout MutableSpan<Int>, _ input: Span<Int>) {
  precondition(output.count >= input.count)
  for i in input.indices {
    output[i] = input[i]
  }
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B16_append_elemwiseyys10OutputSpanVySiGz_s0I0VySiGtF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B16_append_elemwiseyys10OutputSpanVySiGz_s0I0VySiGtF'

// CHECK-IR: define {{.*}} void @"$s31mutable_span_bounds_check_tests0B16_append_elemwiseyys10OutputSpanVySiGz_s0I0VySiGtF"
// CHECK-IR: vector.body
// CHECK-IR: store <{{.*}}>
@_lifetime(output: copy output, copy input)
public func span_append_elemwise(_ output: inout OutputSpan<Int>, _ input: Span<Int>) {
  for i in input.indices {
    output.append(input[i])
  }
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B12_sum_wo_trapyys11MutableSpanVySiGz_s0J0VySiGAHtF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B12_sum_wo_trapyys11MutableSpanVySiGz_s0J0VySiGAHtF'

// CHECK-IR: define {{.*}} void @"$s31mutable_span_bounds_check_tests0B12_sum_wo_trapyys11MutableSpanVySiGz_s0J0VySiGAHtF"
// CHECK-IR: vector.body
// CHECK-IR: store <{{.*}}>
@_lifetime(output: copy output, copy input1, copy input2)
public func span_sum_wo_trap(_ output: inout MutableSpan<Int>, _ input1: Span<Int>, _ input2: Span<Int>) {
  precondition(input1.count == input2.count)
  precondition(output.count == input1.count)
  for i in input1.indices {
    output[i] = input1[i] &+ input2[i]
  }
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B14_sum_with_trapyys11MutableSpanVySiGz_s0J0VySiGAHtF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B14_sum_with_trapyys11MutableSpanVySiGz_s0J0VySiGAHtF'
@_lifetime(output: copy input1, copy input2)
public func span_sum_with_trap(_ output: inout MutableSpan<Int>, _ input1: Span<Int>, _ input2: Span<Int>) {
  precondition(input1.count == input2.count)
  precondition(output.count == input1.count)
  for i in input1.indices {
    output[i] = input1[i] + input2[i]
  }
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B12_bubble_sortyys11MutableSpanVySiGzF :
// CHECK-SIL: bb12{{.*}}:
// TODO-CHECK-SIL-NOT: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B12_bubble_sortyys11MutableSpanVySiGzF'
public func span_bubble_sort(_ span: inout MutableSpan<Int>) {
  if span.count <= 1 {
    return
  }
  for i in 0..<span.count - 1 {
    for j in 0..<span.count - i - 1 {
      if (span[j] > span[j + 1]) {
        let tmp = span[j]
        span[j] = span[j + 1]
        span[j + 1] = tmp
      }
    }
  }
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests6sortedySbs11MutableSpanVySiGF :
// CHECK-SIL: bb4:
// CHECK-SIL: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL: cond_fail {{.*}}, "index out of bounds"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests6sortedySbs11MutableSpanVySiGF'
public func sorted(_ span: borrowing MutableSpan<Int>) -> Bool {
  if span.count <= 1 {
    return true
  }
  for i in 0..<span.count - 1 {
    if (span[i] > span[i + 1]) {
      return false
    }
  }
  return true
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests22outputspan_get_elementySis10OutputSpanVySiG_SitF :
// CHECK-SIL:   [[REG1:%.*]] = struct_extract %0, #OutputSpan._count
// CHECK-SIL:   [[REG2:%.*]] = struct_extract [[REG1]], #Int._value
// CHECK-SIL:   [[REG3:%.*]] = builtin "assumeNonNegative_Int{{32|64}}"([[REG2]]) : $Builtin.Int{{32|64}}
// CHECK-SIL-LABEL:} // end sil function '$s31mutable_span_bounds_check_tests22outputspan_get_elementySis10OutputSpanVySiG_SitF'
public func outputspan_get_element(_ v: borrowing OutputSpan<Int>, _ i: Int) -> Int {
  return v[i]
}

