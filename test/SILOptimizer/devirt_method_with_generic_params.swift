// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

class S<T> {
  func f<U>(_ x: T, _ y: U) -> T { return x }
}

// Check that invocation of a method that has its own
// generic parameter can be devirtualized correctly.
// The body of the method f should be inlined.
// No remaining class_method or apply instructions
// should be present after optimizations are applied.

// CHECK-LABEL: sil @$s33devirt_method_with_generic_params38testDevirtMethodWithItsOwnGenericParamSiyF
// CHECK-NOT: class_method
// CHECK-NOT: apply
// CHECK: return
public func testDevirtMethodWithItsOwnGenericParam() -> Int {
  let s: S<Int> = S<Int>()
  return s.f(0, 0.0)
}
