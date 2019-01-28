// RUN: %target-swift-frontend -emit-sil %s | %FileCheck -check-prefix=CHECK-VJP %s

public class NonTrivialStuff : Equatable {
  public init() {}
  public static func == (lhs: NonTrivialStuff, rhs: NonTrivialStuff) -> Bool { return true }
}

@_fixed_layout
public struct Vector : AdditiveArithmetic, VectorNumeric, Differentiable, Equatable {
  public var x: Float
  public var y: Float
  public var nonTrivialStuff = NonTrivialStuff()
  public typealias TangentVector = Vector
  public typealias CotangentVector = Vector
  public func tangentVector(from cotangent: CotangentVector) -> TangentVector { return cotangent }
  public typealias Scalar = Float
  public static var zero: Vector { return Vector(0) }
  public init(_ scalar: Float) { self.x = scalar; self.y = scalar }
  @differentiable(vjp: fakeVJP)
  public static func + (lhs: Vector, rhs: Vector) -> Vector { abort() }
  @differentiable(vjp: fakeVJP)
  public static func - (lhs: Vector, rhs: Vector) -> Vector { abort() }
  public static func * (lhs: Float, rhs: Vector) -> Vector { abort() }
  public static func fakeVJP(lhs: Vector, rhs: Vector) -> (Vector, (Vector) -> (Vector, Vector)) { abort() }
}

// This exists to minimize generated SIL.
@inline(never) func abort() -> Never { fatalError() }

func testOwnedVector(_ x: Vector) -> Vector {
  return x + x
}
_ = pullback(at: Vector.zero, in: testOwnedVector)

// CHECK-VJP-LABEL: struct {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0 {
// CHECK-VJP-NEXT:   @_hasStorage var pullback_0: (Vector) -> (Vector, Vector)
// CHECK-VJP-NEXT: }

// The primal should not release primal values.
//
// CHECK-VJP-LABEL: @{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0 : $@convention(thin) (@guaranteed Vector) -> (@owned {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, @owned Vector) {
// CHECK-VJP:   [[ADD_VJP:%.*]] = function_ref @{{.*}}fakeVJP{{.*}}
// CHECK-VJP:   [[ADD_VJP_RESULT:%.*]] = apply [[ADD_VJP]]({{.*}}, {{.*}}, {{.*}}) : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> (@owned Vector, @owned @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector))
// CHECK-VJP:   [[ADD_PULLBACK:%.*]] = tuple_extract [[ADD_VJP_RESULT]] : $(Vector, @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector)), 1
// CHECK-VJP-NOT:   release_value [[ADD_VJP_RESULT]]
// CHECK-VJP-NOT:   release_value [[ADD_PULLBACK]]

// The adjoint should not release primal values because they are passed in as @guaranteed.
//
// CHECK-VJP-LABEL: @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK-VJP: bb0({{%.*}} : $Vector, [[PRIMAL_VALUES:%.*]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0):
// CHECK-VJP:   [[PULLBACK0:%.*]] = struct_extract [[PRIMAL_VALUES]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, #{{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0.pullback_0
// CHECK-VJP-NOT:   release_value [[PULLBACK0]]
// CHECK-VJP-NOT:   release_value [[PRIMAL_VALUES]]

// CHECK-NOVJP-LABEL: @{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0 : $@convention(thin) (@guaranteed Vector) -> (@owned {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, @owned Vector) {
// CHECK-NOVJP:   [[ADD:%.*]] = function_ref @$s11refcounting6VectorV1poiyA2C_ACtFZ : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> @owned Vector
// CHECK-NOVJP:   [[ADD_RESULT:%.*]] = apply [[ADD]]({{.*}}, {{.*}}, {{.*}}) : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> @owned Vector
// CHECK-NOVJP-NOT: release_value [[ADD_RESULT]]
// CHECK-NOVJP:   struct ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0 ([[ADD_RESULT]] : $Vector)
// CHECK-NOVJP-NOT: release_value [[ADD_RESULT]]
// The adjoint should not release primal values because they are passed in as @guaranteed.
//
// CHECK-NOVJP-LABEL: @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK-NOVJP: bb0({{%.*}} : $Vector, [[PRIMAL_VALUES:%.*]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0):
// CHECK-NOVJP:   [[PV0:%.*]] = struct_extract [[PRIMAL_VALUES]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, #{{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0.v_0
// CHECK-NOVJP-NOT:   release_value [[PV0]]
// CHECK-NOVJP-NOT:   release_value [[PRIMAL_VALUES]]
