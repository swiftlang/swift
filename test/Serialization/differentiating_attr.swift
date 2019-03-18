// SWIFT_ENABLE_TENSORFLOW

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/differentiating_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -disable-sil-linking -enable-sil-verify-all %t/differentiating_attr.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

func add(x: Float, y: Float) -> Float {
  return x + y
}
// CHECK: @differentiating(add, wrt: x)
// CHECK-NEXT: func jvpAddWrtX(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float))
@differentiating(add, wrt: x)
func jvpAddWrtX(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float)) {
  return (x + y, { $0 })
}
// CHECK: @differentiating(add)
// CHECK-NEXT: func vjpAddWrtXY(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float))
@differentiating(add)
func vjpAddWrtXY(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

func generic<T : Numeric>(x: T) -> T {
  return x
}
// CHECK: @differentiating(generic)
// CHECK-NEXT: func vjpGeneric<T>(x: T) -> (value: T, pullback: (T.CotangentVector) -> T.CotangentVector)
@differentiating(generic)
func vjpGeneric<T>(x: T) -> (value: T, pullback: (T.CotangentVector) -> T.CotangentVector)
  where T : Numeric, T : Differentiable
{
  return (x, { v in v })
}

protocol InstanceMethod : Differentiable {
  func foo(_ x: Self) -> Self
  func bar<T : Differentiable>(_ x: T) -> Self
}
extension InstanceMethod {
  // CHECK: @differentiating(foo)
  // CHECK-NEXT: func vjpFoo(x: Self) -> (value: Self, pullback: (Self.CotangentVector) -> (Self.CotangentVector, Self.CotangentVector))
  @differentiating(foo)
  func vjpFoo(x: Self) -> (value: Self, pullback: (Self.CotangentVector) -> (Self.CotangentVector, Self.CotangentVector)) {
    return (x, { ($0, $0) })
  }

  // CHECK: @differentiating(bar)
  // CHECK-NEXT: func jvpBarWrt<T>(_ x: T) -> (value: Self, differential: (Self.TangentVector, T.TangentVector) -> Self.TangentVector) where T : Differentiable
  @differentiating(bar, wrt: (self, x))
  func jvpBarWrt<T : Differentiable>(_ x: T) -> (value: Self, differential: (Self.TangentVector, T.TangentVector) -> Self.TangentVector) {
    return (self, { dself, dx in dself })
  }
}
