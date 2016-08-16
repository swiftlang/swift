// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// XFAIL: linux

import Darwin

// Make sure we use an intrinsic for functions such as exp.

// CHECK-LABEL: define {{.*}}test1
// CHECK: call float @llvm.exp.f32

public func test1(f : Float) -> Float {
  return exp(f)
}

// CHECK-LABEL: define {{.*}}test2
// CHECK: call double @llvm.exp.f64

public func test2(f : Double) -> Double {
  return _exp(f)
}

// LLVM's sqrt intrinsic does not have the same semantics as libm's sqrt.
// In particular, llvm.sqrt(negative) is documented as being undef, but
// we want sqrt(negative) to be defined to be NaN for IEEE 754 conformance.

// CHECK-LABEL: define {{.*}}test3
// CHECK-NOT: call double @llvm.sqrt.f64

public func test3(d : Double) -> Double {
  return sqrt(d)
}

// CHECK-LABEL: define {{.*}}test4
// CHECK-NOT: call float @llvm.sqrt.f32

public func test4(f : Float) -> Float {
  return sqrt(f)
}
