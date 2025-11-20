// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

class S<T> {
  @inline(__always) func l0(_ x: T) -> T { return x }
  @inline(__always) func l1(_ x: T) -> T { return l0(x) }
  @inline(__always) func l2(_ x: T) -> T { return l1(x) }
  @inline(__always) func l3(_ x: T) -> T { return l2(x) }
  @inline(__always) func l4(_ x: T) -> T { return l3(x) }
  @inline(__always) func l5(_ x: T) -> T { return l4(x) }
  @inline(__always) func l6(_ x: T) -> T { return l5(x) }
  @inline(__always) func l7(_ x: T) -> T { return l6(x) }
  @inline(__always) func l8(_ x: T) -> T { return l7(x) }
  @inline(__always) func l9(_ x: T) -> T { return l8(x) }
}


// CHECK-LABEL: sil @$s11inline_deep3tops5Int32VyF
public func top() -> Int32 {
  // CHECK: bb0
  // CHECK: [[INTLIT:%.*]] = integer_literal $Builtin.Int32, 709
  // CHECK: [[STRUCT:%.*]] = struct $Int32 ([[INTLIT]] : $Builtin.Int32)
  // CHECK-NOT: apply
  return S().l9(709)
  // CHECK: return [[STRUCT]]
}
