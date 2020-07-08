// SWIFT_ENABLE_TENSORFLOW

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/differentiable_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -enable-sil-verify-all %t/differentiable_attr.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

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
  return x + y
}

struct InstanceMethod : Differentiable {
  // CHECK: @differentiable(wrt: (self, y))
  // CHECK-NEXT: func testWrtClause(x: Float, y: Float) -> Float
  @differentiable(wrt: (self, y))
  func testWrtClause(x: Float, y: Float) -> Float {
    return x + y
  }
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

// CHECK: @differentiable(wrt: x where T : Differentiable, T == T.TangentVector)
// CHECK-NEXT: func testWhereClauseMethodTypeConstraint<T>(x: T) -> T where T : Numeric
@differentiable(where T : Differentiable, T == T.TangentVector)
func testWhereClauseMethodTypeConstraint<T : Numeric>(x: T) -> T {
  return x
}

extension P {
  // CHECK: @differentiable(wrt: self where Self : Differentiable, Self == Self.TangentVector)
  // CHECK-NEXT: func testWhereClauseMethodTypeConstraint() -> Self
  @differentiable(wrt: self where Self.TangentVector == Self, Self : Differentiable)
  func testWhereClauseMethodTypeConstraint() -> Self {
    return self
  }
}

// CHECK: func testDifferentiableParam(f: @differentiable (Float) -> Float)
func testDifferentiableParam(f: @differentiable (Float) -> Float) {}
