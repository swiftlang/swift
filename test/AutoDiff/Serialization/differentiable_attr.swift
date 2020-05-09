// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/differentiable_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -disable-sil-linking -enable-sil-verify-all %t/differentiable_attr.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

import _Differentiation

// CHECK: @differentiable(wrt: x)
// CHECK-NEXT: func simple(x: Float) -> Float
@differentiable
func simple(x: Float) -> Float {
  return x
}

// CHECK: @differentiable(linear, wrt: x)
// CHECK-NEXT: func simple2(x: Float) -> Float
@differentiable(linear)
func simple2(x: Float) -> Float {
  return x
}

// CHECK: @differentiable(linear, wrt: x)
// CHECK-NEXT: func simple4(x: Float) -> Float
@differentiable(linear, wrt: x)
func simple4(x: Float) -> Float {
  return x
}

func jvpSimple(x: Float) -> (Float, (Float) -> Float) {
  return (x, { v in v })
}

func vjpSimple(x: Float) -> (Float, (Float) -> Float) {
  return (x, { v in v })
}

// CHECK: @differentiable(wrt: x)
// CHECK-NEXT: func testWrtClause(x: Float, y: Float) -> Float
@differentiable(wrt: x)
func testWrtClause(x: Float, y: Float) -> Float {
  return x
}

struct InstanceMethod : Differentiable {
  // CHECK: @differentiable(wrt: (self, y))
  // CHECK-NEXT: func testWrtClause(x: Float, y: Float) -> Float
  @differentiable(wrt: (self, y))
  func testWrtClause(x: Float, y: Float) -> Float {
    return x
  }

  struct TangentVector: Differentiable, AdditiveArithmetic {
    typealias TangentVector = Self
    static func ==(_: Self, _: Self) -> Bool { fatalError() }
    static var zero: Self { fatalError() }
    static func +(_: Self, _: Self) -> Self { fatalError() }
    static func -(_: Self, _: Self) -> Self { fatalError() }
  }
  mutating func move(along direction: TangentVector) {}
}

// CHECK: @differentiable(wrt: x where T : Differentiable)
// CHECK-NEXT: func testOnlyWhereClause<T>(x: T) -> T where T : Numeric
@differentiable(where T : Differentiable)
func testOnlyWhereClause<T : Numeric>(x: T) -> T {
  return x
}

// CHECK: @differentiable(wrt: x where T : Differentiable)
// CHECK-NEXT: func testWhereClause<T>(x: T) -> T where T : Numeric
@differentiable(where T : Differentiable)
func testWhereClause<T : Numeric>(x: T) -> T {
  return x
}

protocol P {}
extension P {
  // CHECK: @differentiable(wrt: self where Self : Differentiable)
  // CHECK-NEXT: func testWhereClauseMethod() -> Self
  @differentiable(wrt: self where Self : Differentiable)
  func testWhereClauseMethod() -> Self {
    return self
  }
}
extension P where Self : Differentiable {
  func vjpTestWhereClauseMethod() -> (Self, (Self.TangentVector) -> Self.TangentVector) {
    return (self, { v in v })
  }
}

// CHECK: @differentiable(wrt: x where T : Differentiable, T == T.TangentVector)
// CHECK-NEXT: func testWhereClauseMethodTypeConstraint<T>(x: T) -> T where T : Numeric
@differentiable(where T : Differentiable, T == T.TangentVector)
func testWhereClauseMethodTypeConstraint<T : Numeric>(x: T) -> T {
  return x
}
func vjpTestWhereClauseMethodTypeConstraint<T>(x: T) -> (T, (T) -> T)
  where T : Numeric, T : Differentiable, T == T.TangentVector
{
  return (x, { v in v })
}

extension P {
  // CHECK: @differentiable(wrt: self where Self : Differentiable, Self == Self.TangentVector)
  // CHECK-NEXT: func testWhereClauseMethodTypeConstraint() -> Self
  @differentiable(wrt: self where Self.TangentVector == Self, Self : Differentiable)
  func testWhereClauseMethodTypeConstraint() -> Self {
    return self
  }
}
extension P where Self : Differentiable, Self == Self.TangentVector {
  func vjpTestWhereClauseMethodTypeConstraint() -> (Self, (Self.TangentVector) -> Self.TangentVector) {
    return (self, { v in v })
  }
}
