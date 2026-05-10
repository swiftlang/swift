// RUN: %target-swift-frontend -O -emit-sil %s -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-SIL 

public protocol P {
   mutating func mutate(_ other: Self)
}

// CHECK-SIL-LABEL: sil @$s38mutable_span_stdlib_bounds_check_tests0a1_B7_doubleyys11MutableSpanVyxGzAA1PRzlF :
// CHECK-SIL: bb3({{.*}}):
// CHECK-SIL-NOT: end_cow_mutation
// CHECK-SIL-NOT: cond_fail "index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s38mutable_span_stdlib_bounds_check_tests0a1_B7_doubleyys11MutableSpanVyxGzAA1PRzlF'
public func mutable_span_double<T: P>(_ ms: inout MutableSpan<T>) {
  for i in ms.indices {
    ms[i].mutate(ms[i])
  }
}

extension Int : P {
  public mutating func mutate(_ other: Int) {
    self += other
  }
}

// CHECK-SIL-LABEL: sil @$s38mutable_span_stdlib_bounds_check_tests17specializedCalleryySaySiGzF : $@convention(thin) (@inout Array<Int>) -> () {
// CHECK-SIL-NOT: cond_fail "index out of bounds"
// CHECK-SIL-LABEL: } // end sil function '$s38mutable_span_stdlib_bounds_check_tests17specializedCalleryySaySiGzF'
public func specializedCaller(_ array: inout Array<Int>) {
  var mut = array.mutableSpan
  mutable_span_double(&mut)
}

