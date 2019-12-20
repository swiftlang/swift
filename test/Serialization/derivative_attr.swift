// SWIFT_ENABLE_TENSORFLOW

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/derivative_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -disable-sil-linking -enable-sil-verify-all %t/derivative_attr.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

func add(x: Float, y: Float) -> Float {
  return x + y
}
// CHECK: @derivative(of: add, wrt: x)
@derivative(of: add, wrt: x)
func jvpAddWrtX(x: Float, y: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x + y, { $0 })
}
// CHECK: @derivative(of: add, wrt: (x, y))
@derivative(of: add)
func vjpAdd(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x + y, { ($0, $0) })
}

func generic<T : Numeric>(x: T) -> T {
  return x
}
// CHECK: @derivative(of: generic, wrt: x)
@derivative(of: generic)
func vjpGeneric<T>(x: T) -> (value: T, pullback: (T.TangentVector) -> T.TangentVector)
  where T : Numeric, T : Differentiable
{
  return (x, { v in v })
}

protocol InstanceMethod : Differentiable {
  func foo(_ x: Self) -> Self
  func bar<T : Differentiable>(_ x: T) -> Self
}
extension InstanceMethod {
  func foo(_ x: Self) -> Self { self }
  func bar<T : Differentiable>(_ x: T) -> Self { self }
}
extension InstanceMethod {
  // CHECK: @derivative(of: foo, wrt: (self, x))
  @derivative(of: foo)
  func vjpFoo(x: Self) -> (value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)) {
    return (x, { ($0, $0) })
  }

  // CHECK: @derivative(of: bar, wrt: (self, x))
  @derivative(of: bar, wrt: (self, x))
  func jvpBarWrt<T : Differentiable>(_ x: T) -> (value: Self, differential: (TangentVector, T) -> TangentVector)
    where T == T.TangentVector
  {
    return (self, { dself, dx in dself })
  }

  // CHECK: @derivative(of: bar, wrt: (self, x))
  @derivative(of: bar, wrt: (self, x))
  func vjpBarWrt<T : Differentiable>(_ x: T) -> (value: Self, pullback: (TangentVector) -> (TangentVector, T))
    where T == T.TangentVector
  {
    return (self, { v in (v, .zero) })
  }
}

// Test deprecated `@differentiating` attribute.
// For simplicity, `@differentiating` is serialized/deserialized as
// `@derivative` attribute.

func subtract(x: Float, y: Float) -> Float {
  return x - y
}
// CHECK: @derivative(of: subtract, wrt: x)
@differentiating(subtract, wrt: x)
func jvpSubtractWrtX(x: Float, y: Float) -> (value: Float, differential: (Float) -> (Float)) {
  return (x - y, { $0 })
}
// CHECK: @derivative(of: subtract, wrt: (x, y))
@differentiating(subtract)
func vjpSubtract(x: Float, y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (x - y, { ($0, -$0) })
}
