// RUN: %target-swift-frontend -emit-sil -Xllvm -differentiation-skip-folding-autodiff-function-extraction %s | %FileCheck %s

public class NonTrivialStuff : Equatable {
  public init() {}
  public static func == (lhs: NonTrivialStuff, rhs: NonTrivialStuff) -> Bool { return true }
}

@_fixed_layout
public struct Vector : AdditiveArithmetic, VectorProtocol, Differentiable, Equatable {
  public var x: Float
  public var y: Float
  public var nonTrivialStuff = NonTrivialStuff()
  public typealias TangentVector = Vector
  public typealias Scalar = Float
  public static var zero: Vector { return Vector(0) }
  public init(_ scalar: Float) { self.x = scalar; self.y = scalar }

  @_silgen_name("Vector_plus")
  @differentiable(vjp: fakeVJP)
  public static func + (lhs: Vector, rhs: Vector) -> Vector { abort() }

  @_silgen_name("Vector_subtract")
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

// CHECK-LABEL: enum {{.*}}testOwnedVector{{.*}}__Pred__src_0_wrt_0 {
// CHECK-NEXT: }
// CHECK-LABEL: struct {{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0 {
// CHECK-NEXT:   @_hasStorage var pullback_0: (Vector) -> (Vector, Vector) { get set }
// CHECK-NEXT: }

// CHECK-LABEL: sil hidden @{{.*}}UsesMethodOfNoDerivativeMember{{.*}}applied2to{{.*}}__adjoint_src_0_wrt_0_1
// CHECK: bb0([[SEED:%.*]] : $Vector, [[PB_STRUCT:%.*]] : ${{.*}}UsesMethodOfNoDerivativeMember{{.*}}applied2to{{.*}}__PB__src_0_wrt_0_1):
// CHECK:   [[PB:%.*]] = struct_extract [[PB_STRUCT]] : ${{.*}}UsesMethodOfNoDerivativeMember{{.*}}applied2to{{.*}}__PB__src_0_wrt_0_1
// CHECK:   [[NEEDED_COTAN:%.*]] = apply [[PB]]([[SEED]]) : $@callee_guaranteed (@guaranteed Vector) -> @owned Vector
// CHECK:   release_value [[SEED:%.*]] : $Vector

// CHECK-LABEL sil hidden @{{.*}}subset_adjoint_releases_unused_ones{{.*}}__adjoint_src_0_wrt_0
// CHECK: bb0([[SEED:%.*]] : $Vector, [[PB_STRUCT:%.*]] : ${{.*}}subset_adjoint_releases_unused_ones{{.*}}__PB__src_0_wrt_0):
// CHECK:   [[PB0:%.*]] = struct_extract [[PB_STRUCT]] : ${{.*}}subset_adjoint_releases_unused_ones{{.*}}, #{{.*}}subset_adjoint_releases_unused_ones{{.*}}__PB__src_0_wrt_0.pullback_1
// CHECK:   [[NEEDED_COTAN0:%.*]] = apply [[PB0]]([[SEED]]) : $@callee_guaranteed (@guaranteed Vector) -> @owned Vector
// CHECK-NOT:  release_value [[NEEDED_COTAN0]] : $Vector
// CHECK:   [[PB1:%.*]] = struct_extract [[PB_STRUCT]] : ${{.*}}subset_adjoint_releases_unused_ones{{.*}}__PB__src_0_wrt_0, #{{.*}}subset_adjoint_releases_unused_ones{{.*}}__PB__src_0_wrt_0.pullback_0
// CHECK:   [[NEEDED_COTAN1:%.*]] = apply [[PB1]]([[NEEDED_COTAN0]]) : $@callee_guaranteed (@guaranteed Vector) -> @owned Vector
// CHECK:   release_value [[NEEDED_COTAN0]] : $Vector
// CHECK:   release_value [[SEED]] : $Vector
// CHECK:   return [[NEEDED_COTAN1]] : $Vector

// CHECK-LABEL: sil hidden @{{.*}}side_effect_release_zero{{.*}}__adjoint_src_0_wrt_0
// CHECK: bb0([[SEED:%.*]] : $Vector, %1 : ${{.*}}side_effect_release_zero{{.*}}_bb0__PB__src_0_wrt_0):
// CHECK:   [[BUF:%.*]] = alloc_stack $Vector
// CHECK:   [[BUF_ACCESS:%.*]] = begin_access [init] [static] [no_nested_conflict] [[BUF]] : $*Vector
// CHECK:   [[ZERO_GETTER:%.*]] = function_ref @$s11refcounting6VectorV4zeroACvgZ
// CHECK:   [[ZERO:%.*]] = apply [[ZERO_GETTER]]({{%.*}}) : $@convention(method) (@thin Vector.Type) -> @owned Vector
// CHECK:   store [[ZERO]] to [[BUF_ACCESS]] : $*Vector
// CHECK:   retain_value [[SEED:%.*]] : $Vector
// CHECK:   release_value [[SEED:%.*]] : $Vector
// CHECK:   destroy_addr [[BUF]] : $*Vector
// CHECK:   dealloc_stack [[BUF]] : $*Vector
// CHECK: }

// The vjp should not release pullback values.
//
// CHECK-LABEL: sil hidden @{{.*}}testOwnedVector{{.*}}__vjp_src_0_wrt_0 : $@convention(thin) (@guaranteed Vector) -> (@owned Vector, @owned @callee_guaranteed (@guaranteed Vector) -> @owned Vector)
// CHECK:   [[ADD:%.*]] = function_ref @Vector_plus
// CHECK:   [[ADD_JVP:%.*]] = function_ref @{{.*}}Vector_plus__jvp_src_0_wrt_0_1{{.*}}
// CHECK:   [[ADD_VJP:%.*]] = function_ref @{{.*}}fakeVJP{{.*}}
// CHECK:   [[ADD_AD_FUNC:%.*]] = autodiff_function [wrt 0 1] [order 1] [[ADD]] {{.*}} with {[[ADD_JVP]] {{.*}}, [[ADD_VJP]] {{.*}}}
// CHECK:   [[ADD_AD_FUNC_EXTRACT:%.*]] = autodiff_function_extract [vjp] [order 1] [[ADD_AD_FUNC]]
// CHECK:   [[ADD_VJP_RESULT:%.*]] = apply [[ADD_AD_FUNC_EXTRACT]]({{.*}}, {{.*}}, {{.*}}) : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> (@owned Vector, @owned @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector))
// CHECK:   [[ADD_PULLBACK:%.*]] = tuple_extract [[ADD_VJP_RESULT]] : $(Vector, @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector)), 1
// CHECK-NOT:   release_value [[ADD_VJP_RESULT]]
// CHECK-NOT:   release_value [[ADD_PULLBACK]]

// The adjoint should not release pullback struct argument because it has @guaranteed convention.
//
// CHECK-LABEL: @{{.*}}testOwnedVector{{.*}}__adjoint_src_0_wrt_0
// CHECK: bb0({{%.*}} : $Vector, [[PB_STRUCT:%.*]] : ${{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0):
// CHECK:   [[PULLBACK0:%.*]] = struct_extract [[PB_STRUCT]] : ${{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0, #{{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0.pullback_0
// CHECK-NOT:   release_value [[PULLBACK0]]
// CHECK-NOT:   release_value [[PB_STRUCT]]
// CHECK: }


func side_effect_release_zero(_ x: Vector) -> Vector {
  var a = x
  a = a + x
  a = a - a
  return a
}
_ = pullback(at: Vector.zero, in: side_effect_release_zero)

func subset_adjoint_releases_unused_ones(_ x: Vector) -> Vector {
  let y = x + .zero
  return .zero + y
}
_ = pullback(at: .zero, in: subset_adjoint_releases_unused_ones)

struct FakeMaxPool : Differentiable {
  @differentiable(wrt: (self, input))
  func applied(to input: Vector) -> Vector { return input }
}

struct UsesMethodOfNoDerivativeMember : Differentiable {
  @noDerivative var maxPool = FakeMaxPool()

  func applied(to input: Vector) -> Vector {
    return maxPool.applied(to: input)
  }
}

_ = UsesMethodOfNoDerivativeMember().pullback(at: .zero) { $0.applied(to: $1) }
