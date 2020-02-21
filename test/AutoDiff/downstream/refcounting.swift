// RUN: %target-swift-frontend -emit-sil -Xllvm -debug-only=differentiation 2>&1 %s | %FileCheck %s -check-prefix=CHECK-DATA-STRUCTURES
// RUN: %target-swift-frontend -emit-sil -Xllvm -differentiation-skip-folding-differentiable-function-extraction %s | %FileCheck %s
// REQUIRES: asserts

public class NonTrivialStuff : Equatable {
  public init() {}
  public static func == (lhs: NonTrivialStuff, rhs: NonTrivialStuff) -> Bool { return true }
}

@frozen
public struct Vector : AdditiveArithmetic, Differentiable, Equatable {
  public var x: Float
  public var y: Float
  public var nonTrivialStuff = NonTrivialStuff()
  public typealias TangentVector = Vector
  public typealias VectorSpaceScalar = Float
  public static var zero: Vector { return Vector(0) }
  public init(_ scalar: Float) { self.x = scalar; self.y = scalar }

  @_silgen_name("Vector_plus")
  @differentiable
  public static func + (lhs: Vector, rhs: Vector) -> Vector { abort() }

  @_silgen_name("Vector_subtract")
  @differentiable
  public static func - (lhs: Vector, rhs: Vector) -> Vector { abort() }

  public func adding(_ scalar: Float) -> Vector { abort() }
  public func subtracting(_ scalar: Float) -> Vector { abort() }
  public func scaled(by scalar: Float) -> Vector { abort() }

  @derivative(of: +)
  @derivative(of: -)
  public static func fakeVJP(lhs: Vector, rhs: Vector) -> (value: Vector, pullback: (Vector) -> (Vector, Vector)) { abort() }
}

// This exists to minimize generated SIL.
@inline(never) func abort() -> Never { fatalError() }

func testOwnedVector(_ x: Vector) -> Vector {
  return x + x
}
_ = pullback(at: Vector.zero, in: testOwnedVector)

// CHECK-DATA-STRUCTURES-LABEL: struct {{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES-NEXT:   var pullback_0: (Vector) -> (Vector, Vector)
// CHECK-DATA-STRUCTURES-NEXT: }
// CHECK-DATA-STRUCTURES-LABEL: enum {{.*}}testOwnedVector{{.*}}__Pred__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES-NEXT: }

// CHECK-LABEL: sil private @{{.*}}UsesMethodOfNoDerivativeMember{{.*}}applied2to{{.*}}__pullback_src_0_wrt_0_1
// CHECK: bb0([[SEED:%.*]] : $Vector, [[PB_STRUCT:%.*]] : ${{.*}}UsesMethodOfNoDerivativeMember{{.*}}applied2to{{.*}}__PB__src_0_wrt_0_1):
// CHECK:   [[PB:%.*]] = struct_extract [[PB_STRUCT]] : ${{.*}}UsesMethodOfNoDerivativeMember{{.*}}applied2to{{.*}}__PB__src_0_wrt_0_1
// CHECK:   [[NEEDED_COTAN:%.*]] = apply [[PB]]([[SEED]]) : $@callee_guaranteed (@guaranteed Vector) -> @owned Vector

// CHECK-LABEL: sil private @{{.*}}subset_pullback_releases_unused_ones{{.*}}__pullback_src_0_wrt_0
// CHECK: bb0([[SEED:%.*]] : $Vector, [[PB_STRUCT:%.*]] : ${{.*}}subset_pullback_releases_unused_ones{{.*}}__PB__src_0_wrt_0):
// CHECK:   [[PB1:%.*]] = struct_extract [[PB_STRUCT]] : ${{.*}}subset_pullback_releases_unused_ones{{.*}}__PB__src_0_wrt_0, #{{.*}}subset_pullback_releases_unused_ones{{.*}}__PB__src_0_wrt_0.pullback_0
// CHECK:   [[PB0:%.*]] = struct_extract [[PB_STRUCT]] : ${{.*}}subset_pullback_releases_unused_ones{{.*}}, #{{.*}}subset_pullback_releases_unused_ones{{.*}}__PB__src_0_wrt_0.pullback_1
// CHECK:   [[NEEDED_COTAN0:%.*]] = apply [[PB0]]([[SEED]]) : $@callee_guaranteed (@guaranteed Vector) -> @owned Vector
// CHECK:   strong_release [[PB0]]
// CHECK-NOT:  release_value [[NEEDED_COTAN0]] : $Vector
// CHECK:   [[NEEDED_COTAN1:%.*]] = apply [[PB1]]([[NEEDED_COTAN0]]) : $@callee_guaranteed (@guaranteed Vector) -> @owned Vector
// CHECK:   strong_release [[PB1]]
// CHECK:   retain_value [[NEEDED_COTAN1]] : $Vector
// CHECK:   release_value [[NEEDED_COTAN0]] : $Vector
// CHECK:   release_value [[NEEDED_COTAN1]] : $Vector
// CHECK:   return [[NEEDED_COTAN1]] : $Vector

// CHECK-LABEL: sil private @{{.*}}side_effect_release_zero{{.*}}__pullback_src_0_wrt_0
// CHECK: bb0([[SEED:%.*]] : $Vector, %1 : ${{.*}}side_effect_release_zero{{.*}}_bb0__PB__src_0_wrt_0):
// CHECK:   [[BUF:%.*]] = alloc_stack $Vector
// CHECK:   [[ZERO_GETTER:%.*]] = function_ref @$s11refcounting6VectorV4zeroACvgZ
// CHECK:   [[ZERO:%.*]] = apply [[ZERO_GETTER]]({{%.*}}) : $@convention(method) (@thin Vector.Type) -> @owned Vector
// CHECK:   store [[ZERO]] to [[BUF]] : $*Vector
// CHECK:   load [[BUF]] : $*Vector
// CHECK:   [[ZERO_GETTER:%.*]] = function_ref @$s11refcounting6VectorV4zeroACvgZ
// CHECK:   [[ZERO:%.*]] = apply [[ZERO_GETTER]]({{%.*}}) : $@convention(method) (@thin Vector.Type) -> @owned Vector
// CHECK:   store [[ZERO]] to [[BUF]] : $*Vector
// CHECK:   retain_value [[SEED:%.*]] : $Vector
// CHECK:   release_value [[SEED:%.*]] : $Vector
// CHECK:   destroy_addr [[BUF]] : $*Vector
// CHECK:   dealloc_stack [[BUF]] : $*Vector
// CHECK: }

// The vjp should not release pullback values.
//
// CHECK-LABEL: sil private @{{.*}}testOwnedVector{{.*}}__vjp_src_0_wrt_0 : $@convention(thin) (@guaranteed Vector) -> (@owned Vector, @owned @callee_guaranteed (@guaranteed Vector) -> @owned Vector)
// CHECK:   [[ADD:%.*]] = function_ref @Vector_plus
// CHECK:   [[ADD_JVP:%.*]] = differentiability_witness_function [jvp] [parameters 0 1] [results 0] @Vector_plus
// CHECK:   [[ADD_VJP:%.*]] = differentiability_witness_function [vjp] [parameters 0 1] [results 0] @Vector_plus
// CHECK:   [[ADD_AD_FUNC:%.*]] = differentiable_function [parameters 0 1] [[ADD]] {{.*}} with_derivative {[[ADD_JVP]] {{.*}}, [[ADD_VJP]] {{.*}}}
// CHECK:   [[ADD_AD_FUNC_EXTRACT:%.*]] = differentiable_function_extract [vjp] [[ADD_AD_FUNC]]
// CHECK:   [[ADD_VJP_RESULT:%.*]] = apply [[ADD_AD_FUNC_EXTRACT]]({{.*}}, {{.*}}, {{.*}}) : $@convention(method) (@guaranteed Vector, @guaranteed Vector, @thin Vector.Type) -> (@owned Vector, @owned @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector))
// CHECK:   [[ADD_PULLBACK:%.*]] = tuple_extract [[ADD_VJP_RESULT]] : $(Vector, @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector)), 1
// CHECK-NOT:   release_value [[ADD_VJP_RESULT]]
// CHECK-NOT:   release_value [[ADD_PULLBACK]]

// The pullback should not release pullback struct argument because it has @guaranteed convention.
//
// CHECK-LABEL: @{{.*}}testOwnedVector{{.*}}__pullback_src_0_wrt_0
// CHECK: bb0({{%.*}} : $Vector, [[PB_STRUCT:%.*]] : ${{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0):
// CHECK:   [[PULLBACK0:%.*]] = struct_extract [[PB_STRUCT]] : ${{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0, #{{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0.pullback_0
// CHECK-NOT:   release_value [[PULLBACK0]] : @callee_guaranteed (@guaranteed Vector) -> (@owned Vector, @owned Vector)
// CHECK-NOT:   release_value [[PB_STRUCT]] : ${{.*}}testOwnedVector{{.*}}__PB__src_0_wrt_0
// CHECK: }

func side_effect_release_zero(_ x: Vector) -> Vector {
  var a = x
  a = a + x
  a = a - a
  return a
}
_ = pullback(at: Vector.zero, in: side_effect_release_zero)

func subset_pullback_releases_unused_ones(_ x: Vector) -> Vector {
  let y = x + .zero
  return .zero + y
}
_ = pullback(at: .zero, in: subset_pullback_releases_unused_ones)

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

_ = pullback(at: UsesMethodOfNoDerivativeMember(), .zero) { $0.applied(to: $1) }
