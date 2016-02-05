// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s

class S<T> {
  @inline(__always) func l0(x: T) -> T { return x }
  @inline(__always) func l1(x: T) -> T { return l0(x) }
  @inline(__always) func l2(x: T) -> T { return l1(x) }
  @inline(__always) func l3(x: T) -> T { return l2(x) }
  @inline(__always) func l4(x: T) -> T { return l3(x) }
  @inline(__always) func l5(x: T) -> T { return l4(x) }
  @inline(__always) func l6(x: T) -> T { return l5(x) }
  @inline(__always) func l7(x: T) -> T { return l6(x) }
  @inline(__always) func l8(x: T) -> T { return l7(x) }
  @inline(__always) func l9(x: T) -> T { return l8(x) }
}


// CHECK-LABEL: sil @_TF11inline_deep3topFT_Vs5Int32
public func top() -> Int32 {
  // CHECK: bb0
  // CHECK: [[INTLIT:%.*]] = integer_literal $Builtin.Int32, 709
  // CHECK: [[STRUCT:%.*]] = struct $Int32 ([[INTLIT]] : $Builtin.Int32)
  // CHECK-NOT: apply
  return S().l9(709)
  // CHECK: return [[STRUCT]]
}
