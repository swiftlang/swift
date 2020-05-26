// RUN: %target-swift-frontend  -primary-file %s -O -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend  -primary-file %s -Osize -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend  -primary-file %s -O -emit-ir | %FileCheck -check-prefix=CHECK-IR %s
// RUN: %target-swift-frontend  -primary-file %s -Osize -emit-ir | %FileCheck -check-prefix=CHECK-IR %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

class A {
  func foo(_ x: Int) -> Int { return x }
}

class B : A {
  override func foo(_ x: Int) -> Int { return x + 1 }
}


func donothing(_ x: Int) -> Int { return x }

// CHECK-LABEL: sil {{.*}} [Osize] @{{.*}}test_osize
// CHECK: [[M:%[0-9]+]] = class_method
// CHECK: [[A:%[0-9]+]] = apply [[M]]
// CHECK: return [[A]]
// CHECK-IR: define hidden {{.*}}test_osize{{.*}} [[SIZE_ATTR:#[0-9]+]]
@_optimize(size)
func test_osize(_ a: A) -> Int {
  return donothing(a.foo(27))
}

// CHECK-LABEL: sil {{.*}} [Onone] @{{.*}}test_onone
// CHECK: [[M:%[0-9]+]] = class_method
// CHECK: [[A:%[0-9]+]] = apply [[M]]
// CHECK: [[A2:%[0-9]+]] = apply
// CHECK: return [[A2]]
// CHECK-IR: define hidden {{.*}}test_onone{{.*}} [[NOSIZE_ATTR:#[0-9]+]]
@_optimize(none)
func test_onone(_ a: A) -> Int {
  return donothing(a.foo(27))
}

// CHECK-LABEL: sil {{.*}} [Ospeed] @{{.*}}test_ospeed
// CHECK: [[M:%[0-9]+]] = class_method
// CHECK: [[A:%[0-9]+]] = apply [[M]]
// CHECK: return [[A]]
// CHECK-IR: define hidden {{.*}}test_ospeed{{.*}} [[NOSIZE_ATTR:#[0-9]+]]
@_optimize(speed)
func test_ospeed(_ a: A) -> Int {
  return donothing(a.foo(27))
}


// CHECK-IR: attributes [[SIZE_ATTR]] = { minsize optsize "
// CHECK-IR: attributes [[NOSIZE_ATTR]] = { "
