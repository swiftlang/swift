// SWIFT_ENABLE_TENSORFLOW

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-module -parse-as-library -o %t
// RUN: llvm-bcanalyzer %t/differentiable_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -disable-sil-linking -enable-sil-verify-all %t/differentiable_attr.swiftmodule -o - | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

@differentiable(jvp: jvpSimpleJVP)
func jvpSimple(x: Float) -> Float {
  return x
}

// CHECK-DAG: @differentiable(wrt: x, jvp: jvpSimpleJVP)
// CHECK-DAG: func jvpSimpleJVP(x: Float) -> (Float, (Float) -> Float)
func jvpSimpleJVP(x: Float) -> (Float, (Float) -> Float) {
  return (x, { v in v })
}

@differentiable(vjp: vjpSimpleVJP)
func vjpSimple(x: Float) -> Float {
  return x
}

// CHECK-DAG: @differentiable(wrt: x, vjp: vjpSimpleVJP)
// CHECK-DAG: func vjpSimpleVJP(x: Float) -> (Float, (Float) -> Float)
func vjpSimpleVJP(x: Float) -> (Float, (Float) -> Float) {
  return (x, { v in v })
}

// CHECK-DAG: @differentiable(wrt: x)
// CHECK-DAG: func testWrtClause(x: Float, y: Float) -> Float
@differentiable(wrt: x)
func testWrtClause(x: Float, y: Float) -> Float {
  return x + y
}

struct InstanceMethod : Differentiable {
  // CHECK-DAG: @differentiable(wrt: (self, y))
  // CHECK-DAG: func testWrtClause(x: Float, y: Float) -> Float
  @differentiable(wrt: (self, y))
  func testWrtClause(x: Float, y: Float) -> Float {
    return x + y
  }
}

// CHECK-DAG: @differentiable(wrt: x where T : Differentiable)
// CHECK-DAG: func testOnlyWhereClause<T>(x: T) -> T where T : Numeric
@differentiable(where T : Differentiable)
func testOnlyWhereClause<T : Numeric>(x: T) -> T {
  return x
}

// CHECK-DAG: @differentiable(wrt: x, vjp: vjpTestWhereClause where T : Differentiable)
// CHECK-DAG: func testWhereClause<T>(x: T) -> T where T : Numeric
@differentiable(vjp: vjpTestWhereClause where T : Differentiable)
func testWhereClause<T : Numeric>(x: T) -> T {
  return x
}
func vjpTestWhereClause<T>(x: T) -> (T, (T.CotangentVector) -> T.CotangentVector)
  where T : Numeric, T : Differentiable
{
  return (x, { v in v })
}

protocol P {}
extension P {
  // Note: Method JVP/VJPs that are differentiable wrt self are erased from
  // `@differentiable` attributes for self-reordering thunking.
  // CHECK-DAG: @differentiable(wrt: self where Self : Differentiable)
  // CHECK-DAG: func testWhereClauseMethod() -> Self
  @differentiable(wrt: self, vjp: vjpTestWhereClauseMethod where Self : Differentiable)
  func testWhereClauseMethod() -> Self {
    return self
  }
}
extension P where Self : Differentiable {
  func vjpTestWhereClauseMethod() -> (Self, (Self.CotangentVector) -> Self.CotangentVector) {
    return (self, { v in v })
  }
}

// CHECK-DAG: @differentiable(wrt: x, vjp: vjpTestWhereClauseMethodTypeConstraint where T : Differentiable, T == T.CotangentVector)
// CHECK-DAG: func testWhereClauseMethodTypeConstraint<T>(x: T) -> T where T : Numeric
@differentiable(vjp: vjpTestWhereClauseMethodTypeConstraint where T : Differentiable, T == T.CotangentVector)
func testWhereClauseMethodTypeConstraint<T : Numeric>(x: T) -> T {
  return x
}
func vjpTestWhereClauseMethodTypeConstraint<T>(x: T) -> (T, (T) -> T)
  where T : Numeric, T : Differentiable, T == T.CotangentVector
{
  return (x, { v in v })
}

extension P {
  // Note: Method JVP/VJPs that are differentiable wrt self are erased from
  // `@differentiable` attributes for self-reordering thunking.
  // CHECK-DAG: @differentiable(wrt: self where Self : Differentiable, Self == Self.CotangentVector)
  // CHECK-DAG: func testWhereClauseMethodTypeConstraint() -> Self
  @differentiable(wrt: self, vjp: vjpTestWhereClauseMethodTypeConstraint where Self.CotangentVector == Self, Self : Differentiable)
  func testWhereClauseMethodTypeConstraint() -> Self {
    return self
  }
}
extension P where Self : Differentiable, Self == Self.CotangentVector {
  func vjpTestWhereClauseMethodTypeConstraint() -> (Self, (Self.CotangentVector) -> Self.CotangentVector) {
    return (self, { v in v })
  }
}
