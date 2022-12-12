// RUN: %target-swift-frontend -emit-ir -primary-file %s -enable-experimental-feature VariadicGenerics | %FileCheck %s

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

// REQUIRES: PTRSIZE=64

struct G<T...> {
  // CHECK-LABEL: define hidden swiftcc void @"$s22variadic_generic_types1GV6calleeyyF"(i64 %0, %swift.type* %T)
  // CHECK: ret void
  func callee() {}

  // CHECK-LABEL: define hidden swiftcc void @"$s22variadic_generic_types1GV6calleryyF"(i64 %0, %swift.type* %T)
  // CHECK: call swiftcc void @"$s22variadic_generic_types1GV6calleeyyF"(i64 %0, %swift.type* %T)
  // CHECK: ret void
  func caller() {
    callee()
  }
}
