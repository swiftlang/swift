// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/SpanExtras.swiftmodule %S/Inputs/SpanExtras.swift -enable-builtin-module  -enable-experimental-feature Lifetimes -O
// RUN: %target-swift-frontend -I %t -O -emit-sil %s -enable-experimental-feature Lifetimes -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-SIL 
// RUN: %target-swift-frontend -I %t -O -emit-ir %s -enable-experimental-feature Lifetimes -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-IR

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

import SpanExtras

// Bounds check should be eliminated
// SIL removes lower bounds check
// LLVM removes upper bounds check and vectorizes the loop

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B10_zero_inityy10SpanExtras07MutableH0VySiGzF : 
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL-NOT: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B10_zero_inityy10SpanExtras07MutableH0VySiGzF'

// CHECK-IR: define {{.*}} void @"$s31mutable_span_bounds_check_tests0B10_zero_inityy10SpanExtras07MutableH0VySiGzF"
// CHECK-IR: vector.body
// CHECK-IR: store <{{.*}}> zeroinitializer,
public func span_zero_init(_ output: inout MutableSpan<Int>) {
  for i in output.indices {
    output[i] = 0
  }
}

// Bounds check should be eliminated
// SIL removes lower bounds check
// LLVM removes upper bounds check and vectorizes the loop

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B14_copy_elemwiseyy10SpanExtras07MutableH0VySiGz_s0H0VySiGtF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL-NOT: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B14_copy_elemwiseyy10SpanExtras07MutableH0VySiGz_s0H0VySiGtF'

// CHECK-IR: define {{.*}} void @"$s31mutable_span_bounds_check_tests0B14_copy_elemwiseyy10SpanExtras07MutableH0VySiGz_s0H0VySiGtF"
// CHECK-IR: vector.body
// CHECK-IR: store <{{.*}}>
@_lifetime(output: copy output, copy input)
public func span_copy_elemwise(_ output: inout MutableSpan<Int>, _ input: Span<Int>) {
  precondition(output.count >= input.count)
  for i in input.indices {
    output[i] = input[i]
  }
}

// Bounds check should be eliminated
// SIL removes lower bounds check
// LLVM removes upper bounds check and vectorizes the loop

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B16_append_elemwiseyy10SpanExtras06OutputH0VySiGz_s0H0VySiGtF : 
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL-NOT: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B16_append_elemwiseyy10SpanExtras06OutputH0VySiGz_s0H0VySiGtF'

// CHECK-IR: define {{.*}} void @"$s31mutable_span_bounds_check_tests0B16_append_elemwiseyy10SpanExtras06OutputH0VySiGz_s0H0VySiGtF"
// CHECK-IR: vector.body
// CHECK-IR: store <{{.*}}>
@_lifetime(output: copy output, copy input)
public func span_append_elemwise(_ output: inout OutputSpan<Int>, _ input: Span<Int>) {
  for i in input.indices {
    output.append(input[i])
  }
}

// Bounds check should be eliminated

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B12_sum_wo_trapyy10SpanExtras07MutableI0VySiGz_s0I0VySiGAItF : 
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL-NOT: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B12_sum_wo_trapyy10SpanExtras07MutableI0VySiGz_s0I0VySiGAItF'

// CHECK-IR: define {{.*}} void @"$s31mutable_span_bounds_check_tests0B12_sum_wo_trapyy10SpanExtras07MutableI0VySiGz_s0I0VySiGAItF"
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

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B14_sum_with_trapyy10SpanExtras07MutableI0VySiGz_s0I0VySiGAItF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL-NOT: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B14_sum_with_trapyy10SpanExtras07MutableI0VySiGz_s0I0VySiGAItF'
@_lifetime(output: copy input1, copy input2)
public func span_sum_with_trap(_ output: inout MutableSpan<Int>, _ input1: Span<Int>, _ input2: Span<Int>) {
  precondition(input1.count == input2.count)
  precondition(output.count == input1.count)
  for i in input1.indices {
    output[i] = input1[i] + input2[i]
  }
}

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests0B12_bubble_sortyy10SpanExtras07MutableH0VySiGzF : 
// CHECK-SIL: bb11({{.*}}):
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests0B12_bubble_sortyy10SpanExtras07MutableH0VySiGzF'
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

// CHECK-SIL-LABEL: sil @$s31mutable_span_bounds_check_tests6sortedySb10SpanExtras07MutableG0VySiGF : 
// CHECK-SIL: bb4:
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_fail {{.*}}, "precondition failure"
// CHECK-SIL: cond_br
// CHECK-SIL-LABEL: } // end sil function '$s31mutable_span_bounds_check_tests6sortedySb10SpanExtras07MutableG0VySiGF'
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

