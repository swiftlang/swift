// RUN: %target-swift-frontend -emit-sil %s | %FileCheck -check-prefix=CHECK-VJP %s
// RUN: %target-swift-frontend -emit-sil -Xllvm -differentiation-use-vjp=false %s | %FileCheck -check-prefix=CHECK-NOVJP %s

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
  public typealias Scalar = Float
  public typealias Shape = ()
  public static var zero: Vector { return Vector(0) }
  public init(repeating repeatedValue: Float, shape: ()) {
    self.init(repeatedValue)
  }
  public init(_ scalar: Float) { self.x = scalar; self.y = scalar }
  @differentiable(reverse, adjoint: fakeAdj)
  public static func + (lhs: Vector, rhs: Vector) -> Vector { abort() }
  @differentiable(reverse, adjoint: fakeAdj)
  public static func - (lhs: Vector, rhs: Vector) -> Vector { abort() }
  public static func * (lhs: Float, rhs: Vector) -> Vector { abort() }
  public static func fakeAdj(lhs: Vector, rhs: Vector, y: Vector, seed: Vector) -> (Vector, Vector) { abort() }
}

// This exists to minimize generated SIL.
@inline(never) func abort() -> Never { fatalError() }

func testOwnedVector(_ x: Vector) -> Vector {
  return x + x
}
_ = #gradient(testOwnedVector)

// CHECK-VJP-LABEL: struct {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0 {
// CHECK-VJP-NEXT:   @sil_stored @usableFromInline
// CHECK-VJP-NEXT:   var v_0: Vector
// CHECK-VJP-NEXT:   @sil_stored @usableFromInline
// CHECK-VJP-NEXT:   var pullback_0: (Vector) -> (Vector, Vector)
// CHECK-VJP-NEXT: }

// CHECK-VJP-LABEL: @{{.*}}testOwnedVector{{.*}}__grad_src_0_wrt_0_s_p
// CHECK-VJP:   [[PRIMAL:%.*]] = function_ref @{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0
// CHECK-VJP:   [[PRIMAL_RESULT:%.*]] = apply [[PRIMAL]]({{.*}})
// CHECK-VJP:   [[NESTED_PRIMAL_VALUES:%.*]] = tuple_extract [[PRIMAL_RESULT]] : $({{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, Vector), 0
// CHECK-VJP:   [[ORIGINAL_RESULT:%.*]] = tuple_extract [[PRIMAL_RESULT]] : $({{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, Vector), 1
// CHECK-VJP:   [[ADJOINT:%.*]] = function_ref @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK-VJP:   {{.*}} = apply [[ADJOINT]]({{.*}}, [[NESTED_PRIMAL_VALUES]], [[ORIGINAL_RESULT]], {{.*}})
// CHECK-VJP:   release_value [[NESTED_PRIMAL_VALUES]]
// CHECK-VJP:   release_value [[ORIGINAL_RESULT]]

// The primal should not release primal values.
//
// CHECK-VJP-LABEL: @{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0 : $@convention(thin) (@guaranteed Vector) -> (@owned {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, @owned Vector) {
// CHECK-VJP:   [[ADD_VJP:%.*]] = function_ref @AD__$s11refcounting6VectorV1poiyA2C_ACtFZ__vjp_src_0_wrt_0_1 : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> (@owned Vector, @owned @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector))
// CHECK-VJP:   [[ADD_VJP_RESULT:%.*]] = apply [[ADD_VJP]]({{.*}}, {{.*}}, {{.*}}) : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> (@owned Vector, @owned @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector))
// CHECK-VJP:   [[ADD_ORIGINAL_RESULT:%.*]] = tuple_extract [[ADD_VJP_RESULT]] : $(Vector, @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector)), 0
// CHECK-VJP:   [[ADD_PULLBACK:%.*]] = tuple_extract [[ADD_VJP_RESULT]] : $(Vector, @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector)), 1
// CHECK-VJP-NOT:   release_value [[ADD_VJP_RESULT]]
// CHECK-VJP-NOT:   release_value [[ADD_ORIGINAL_RESULT]]
// CHECK-VJP-NOT:   release_value [[ADD_PULLBACK]]
// CHECK-VJP:   } // end sil function '{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0'

// The adjoint should not release primal values because they are passed in as @guaranteed.
//
// CHECK-VJP-LABEL: @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK-VJP: bb0({{%.*}} : $Vector, [[PRIMAL_VALUES:%.*]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, {{%.*}} : $Vector, {{%.*}} : $Vector):
// CHECK-VJP:   [[PULLBACK0:%.*]] = struct_extract [[PRIMAL_VALUES]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, #{{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0.pullback_0
// CHECK-VJP-NOT:   release_value [[PULLBACK0]]
// CHECK-VJP-NOT:   release_value [[PRIMAL_VALUES]]
// CHECK-VJP:   } // end sil function '{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0'

// CHECK-NOVJP-LABEL: struct {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0 {
// CHECK-NOVJP-NEXT:   @sil_stored @usableFromInline
// CHECK-NOVJP-NEXT:   var v_0: Vector
// CHECK-NOVJP-NEXT: }
// CHECK-NOVJP-LABEL: @{{.*}}testOwnedVector{{.*}}__grad_src_0_wrt_0_s_p
// CHECK-NOVJP:   [[PRIMAL:%.*]] = function_ref @{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0
// CHECK-NOVJP:   [[PRIMAL_RESULT:%.*]] = apply [[PRIMAL]]({{.*}})
// CHECK-NOVJP:   [[NESTED_PRIMAL_VALUES:%.*]] = tuple_extract [[PRIMAL_RESULT]] : $({{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, Vector), 0
// CHECK-NOVJP:   [[ORIGINAL_RESULT:%.*]] = tuple_extract [[PRIMAL_RESULT]] : $({{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, Vector), 1
// CHECK-NOVJP:   [[ADJOINT:%.*]] = function_ref @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK-NOVJP:   {{.*}} = apply [[ADJOINT]]({{.*}}, [[NESTED_PRIMAL_VALUES]], [[ORIGINAL_RESULT]], {{.*}})
// CHECK-NOVJP:   release_value [[NESTED_PRIMAL_VALUES]]
// CHECK-NOVJP:   release_value [[ORIGINAL_RESULT]]
// The primal should not release primal values.
//
// CHECK-NOVJP-LABEL: @{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0 : $@convention(thin) (@guaranteed Vector) -> (@owned {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, @owned Vector) {
// CHECK-NOVJP:   [[ADD:%.*]] = function_ref @$s11refcounting6VectorV1poiyA2C_ACtFZ : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> @owned Vector
// CHECK-NOVJP:   [[ADD_RESULT:%.*]] = apply [[ADD]]({{.*}}, {{.*}}, {{.*}}) : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> @owned Vector
// CHECK-NOVJP-NOT: release_value [[ADD_RESULT]]
// CHECK-NOVJP:   struct ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0 ([[ADD_RESULT]] : $Vector)
// CHECK-NOVJP-NOT: release_value [[ADD_RESULT]]
// The adjoint should not release primal values because they are passed in as @guaranteed.
//
// CHECK-NOVJP-LABEL: @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK-NOVJP: bb0({{%.*}} : $Vector, [[PRIMAL_VALUES:%.*]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, {{%.*}} : $Vector, {{%.*}} : $Vector):
// CHECK-NOVJP:   [[PV0:%.*]] = struct_extract [[PRIMAL_VALUES]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, #{{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0.v_0
// CHECK-NOVJP-NOT:   release_value [[PV0]]
// CHECK-NOVJP-NOT:   release_value [[PRIMAL_VALUES]]
