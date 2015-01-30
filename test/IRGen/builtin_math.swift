// RUN: %target-swift-frontend -emit-ir %s | FileCheck %s

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

// CHECK-LABEL: define {{.*}}test3
// CHECK: call double @sqrt

public func test3(d : Double) -> Double {
  return sqrt(d)
}

// CHECK-LABEL: define {{.*}}test4
// CHECK: call float @sqrtf

public func test4(f : Float) -> Float {
  return sqrt(f)
}
