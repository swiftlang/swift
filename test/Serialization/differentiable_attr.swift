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

// CHECK-DAG: @differentiable(wrt: (.0), jvp: jvpSimpleJVP)
// CHECK-DAG: func jvpSimpleJVP(x: Float) -> (Float, (Float) -> Float)
func jvpSimpleJVP(x: Float) -> (Float, (Float) -> Float) {
  return (x, { v in v })
}

@differentiable(vjp: vjpSimpleVJP)
func vjpSimple(x: Float) -> Float {
  return x
}

// CHECK-DAG: @differentiable(wrt: (.0), vjp: vjpSimpleVJP)
// CHECK-DAG: func vjpSimpleVJP(x: Float) -> (Float, (Float) -> Float)
func vjpSimpleVJP(x: Float) -> (Float, (Float) -> Float) {
  return (x, { v in v })
}

// CHECK-DAG: @differentiable(wrt: (.0), vjp: vjpTestWhereClause where T : Differentiable, T : Numeric)
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
  // CHECK-DAG: @differentiable(wrt: (self), vjp: vjpTestWhereClause where Self : Differentiable, Self : P)
  // CHECK-DAG: func testWhereClause() -> Self
  @differentiable(wrt: (self), vjp: vjpTestWhereClause where Self : Differentiable)
  func testWhereClause() -> Self {
    return self
  }
}
extension P where Self : Differentiable {
  func vjpTestWhereClause() -> (Self, (Self.CotangentVector) -> Self.CotangentVector) {
    return (self, { v in v })
  }
}

// NOTE: The failing tests involve where clauses with member type constraints.
// They pass type-checking but crash during serialization.

// CHECK-DAG: @differentiable(wrt: (.0), vjp: vjpTestWhereClauseMemberTypeConstraint where T : Differentiable, T : Numeric, T == T.CotangentVector)
// CHECK-DAG: func testWhereClauseMemberTypeConstraint<T>(x: T) -> T where T : Numeric
@differentiable(vjp: vjpTestWhereClauseMemberTypeConstraint where T : Differentiable, T == T.CotangentVector)
func testWhereClauseMemberTypeConstraint<T : Numeric>(x: T) -> T {
  return x
}
func vjpTestWhereClauseMemberTypeConstraint<T>(x: T) -> (T, (T) -> T)
  where T : Numeric, T : Differentiable, T == T.CotangentVector
{
  return (x, { v in v })
}

extension P {
  // CHECK-DAG: @differentiable(wrt: (self), vjp: vjpTestWhereClauseMemberTypeConstraint where Self : Differentiable, Self : P, Self == Self.CotangentVector)
  // CHECK-DAG: func testWhereClauseMemberTypeConstraint() -> Self
  @differentiable(wrt: (self), vjp: vjpTestWhereClauseMemberTypeConstraint where Self.CotangentVector == Self, Self : Differentiable)
  func testWhereClauseMemberTypeConstraint() -> Self {
    return self
  }
}
extension P where Self : Differentiable, Self == Self.CotangentVector {
  func vjpTestWhereClauseMemberTypeConstraint() -> (Self, (Self.CotangentVector) -> Self.CotangentVector) {
    return (self, { v in v })
  }
}
