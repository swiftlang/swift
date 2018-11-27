// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

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

// CHECK-LABEL: struct {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0 {
// CHECK-NEXT:   @sil_stored @usableFromInline
// CHECK-NEXT:   var v_0: Vector
// CHECK-NEXT: }

// CHECK-LABEL: @{{.*}}testOwnedVector{{.*}}__grad_src_0_wrt_0_s_p
// CHECK:   [[PRIMAL:%.*]] = function_ref @{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0
// CHECK:   [[PRIMAL_RESULT:%.*]] = apply [[PRIMAL]]({{.*}})
// CHECK:   [[NESTED_PRIMAL_VALUES:%.*]] = tuple_extract [[PRIMAL_RESULT]] : $({{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, Vector), 0
// CHECK:   [[ORIGINAL_RESULT:%.*]] = tuple_extract [[PRIMAL_RESULT]] : $({{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, Vector), 1
// CHECK:   [[ADJOINT:%.*]] = function_ref @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK:   {{.*}} = apply [[ADJOINT]]({{.*}}, [[NESTED_PRIMAL_VALUES]], [[ORIGINAL_RESULT]], {{.*}})
// CHECK:   release_value [[NESTED_PRIMAL_VALUES]]
// CHECK:   release_value [[ORIGINAL_RESULT]]

// The primal should not release primal values.
//
// CHECK-LABEL: @{{.*}}testOwnedVector{{.*}}__primal_src_0_wrt_0 : $@convention(thin) (@guaranteed Vector) -> (@owned {{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, @owned Vector) {
// CHECK:   [[ADD:%.*]] = function_ref @$s11refcounting6VectorV1poiyA2C_ACtFZ : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> @owned Vector
// CHECK:   [[ADD_RESULT:%.*]] = apply [[ADD]]({{.*}}, {{.*}}, {{.*}}) : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> @owned Vector
// CHECK-NOT: release_value [[ADD_RESULT]]
// CHECK:   struct ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0 ([[ADD_RESULT]] : $Vector)
// CHECK-NOT: release_value [[ADD_RESULT]]

// The adjoint should not release primal values because they are passed in as @guaranteed.
//
// CHECK-LABEL: @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK: bb0({{%.*}} : $Vector, {{%.*}} : $Vector, [[PRIMAL_VALUES:%.*]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, {{%.*}} : $Vector):
// CHECK:   [[PV0:%.*]] = struct_extract [[PRIMAL_VALUES]] : ${{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0, #{{.*}}testOwnedVector{{.*}}__Type__src_0_wrt_0.v_0
// CHECK-NOT:   release_value [[PV0]]
// CHECK-NOT:   release_value [[PRIMAL_VALUES]]
