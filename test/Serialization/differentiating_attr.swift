// SWIFT_ENABLE_TENSORFLOW

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/differentiating_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -disable-sil-linking -enable-sil-verify-all %t/differentiating_attr.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

// CHECK: @differentiable(wrt: x, jvp: jvpAddWrtX)
// CHECK-NEXT: @differentiable(vjp: vjpAdd)
func add(x: Float, y: Float) -> Float {
  return x + y
}
@differentiating(add, wrt: x)
func jvpAddWrtX(x: Float, y: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x + y, { $0 })
}
@differentiating(add)
func vjpAdd(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

// CHECK: @differentiable(vjp: vjpGeneric where T : Differentiable)
func generic<T : Numeric>(x: T) -> T {
  return x
}
@differentiating(generic)
func vjpGeneric<T>(x: T) -> (value: T, pullback: (T.CotangentVector) -> T.CotangentVector)
  where T : Numeric, T : Differentiable
{
  return (x, { v in v })
}

protocol InstanceMethod : Differentiable {
  // CHECK: @differentiable(vjp: vjpFoo)
  func foo(_ x: Self) -> Self
  // CHECK: @differentiable(jvp: jvpBarWrt where T == T.TangentVector)
  func bar<T : Differentiable>(_ x: T) -> Self
}
extension InstanceMethod {
  @differentiating(foo)
  func vjpFoo(x: Self) -> (value: Self, pullback: (Self.CotangentVector) -> (Self.CotangentVector, Self.CotangentVector)) {
    return (x, { ($0, $0) })
  }

  @differentiating(bar, wrt: (self, x))
  func jvpBarWrt<T : Differentiable>(_ x: T) -> (value: Self, differential: (Self.TangentVector, T) -> Self.TangentVector)
    where T == T.TangentVector
  {
    return (self, { dself, dx in dself })
  }
}
