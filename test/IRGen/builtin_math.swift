// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -O %s | %FileCheck %s

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
// CHECK: call double @sqrt

public func test3(d : Double) -> Double {
  return sqrt(d)
}

// CHECK-LABEL: define {{.*}}test4
// CHECK: call float @sqrtf

public func test4(f : Float) -> Float {
  return sqrt(f)
}

// CHECK-LABEL: define {{.*}}test3a
// CHECK: call double @remainder

public func test3a(d : Double) -> Double {
  return remainder(1,d)
}

// CHECK-LABEL: define {{.*}}test4a
// CHECK: call float @remainder

public func test4a(f : Float) -> Float {
  return remainder(1,f)
}

// CHECK-LABEL: define {{.*}}test5
// CHECK: ret float 2

public func test5( ) -> Float {
  return sqrt(4)
}

// CHECK-LABEL: define {{.*}}test6
// CHECK: ret double 2

public func test6( ) -> Double {
  return sqrt(4)
}

// CHECK-LABEL: define {{.*}}test7
// CHECK-NOT: ret float undef

public func test7( ) -> Float {
  return sqrt(-1)
}

// CHECK-LABEL: define {{.*}}test8
// CHECK-NOT: ret double undef

public func test8( ) -> Double {
  return sqrt(-1)
}
