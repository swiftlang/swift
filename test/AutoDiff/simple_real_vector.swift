// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

@_fixed_layout
public struct Vector : VectorNumeric {
  public var x: Float
  public var y: Float

  public typealias ScalarElement = Float
  public typealias Dimensionality = ()

  public init(dimensionality: (), repeating repeatedValue: Float) {
    self.init(repeatedValue)
  }

  public init(_ scalar: Float) {
    self.x = scalar
    self.y = scalar
  }
}

// This exists to minimize generated SIL.
@inline(never) func abort() -> Never { fatalError() }

@differentiable(reverse, adjoint: fakeAdj)
public func + (lhs: Vector, rhs: Vector) -> Vector {
  abort()
}
@differentiable(reverse, adjoint: fakeAdj)
public func - (lhs: Vector, rhs: Vector) -> Vector {
  abort()
}
@differentiable(reverse, adjoint: fakeAdj)
public func * (lhs: Vector, rhs: Vector) -> Vector {
  abort()
}

public func fakeAdj(lhs: Vector, rhs: Vector, y: Vector, seed: Vector) -> (Vector, Vector) {
  abort()
}

public func test1() -> Vector {
  func foo(_ x: Vector) -> Vector {
    return x + x
  }
  return #gradient(foo)(Vector(10))
}

// CHECK-LABEL: sil private @{{.*}}test1{{.*}}foo{{.*}}__grad_src_0_wrt_0 : $@convention(thin) (Vector) -> Vector {
// CHECK:   [[BUILTIN_SCALAR_INIT_METHOD:%.*]] = witness_method $Int64, #_ExpressibleByBuiltinIntegerLiteral.init!allocator.1
// CHECK:   apply [[BUILTIN_SCALAR_INIT_METHOD]]<Int64>({{%.*}}, {{%.*}}, {{%.*}})
// CHECK:   [[SCALAR_INIT_METHOD:%.*]] = witness_method $Float, #ExpressibleByIntegerLiteral.init!allocator.1
// CHECK:   apply [[SCALAR_INIT_METHOD]]<Float>({{%.*}}, {{%.*}}, {{%.*}})
// CHECK:   [[VECTOR_INIT_METHOD:%.*]] = witness_method $Vector, #VectorNumeric.init!allocator.1
// CHECK:   apply [[VECTOR_INIT_METHOD]]<Vector>(%1, %16, %15) : $@convention(witness_method: VectorNumeric) <τ_0_0 where τ_0_0 : VectorNumeric> (@in τ_0_0.ScalarElement, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[CAN_GRAD:%.*]] = function_ref @{{.*}}test1{{.*}}foo{{.*}}__grad_src_0_wrt_0_s_p : $@convention(thin) (Vector, Vector) -> (Vector, Vector)
// CHECK:   apply [[CAN_GRAD]]({{.*}}, {{.*}})

// CHECK-LABEL: sil private @{{.*}}test1{{.*}}foo{{.*}}__adjoint_src_0_wrt_0
// CHECK: bb0(%0 : $Vector, %1 : ${{.*}}test1{{.*}}foo{{.*}}__Type{{.*}}, %2 : $Vector, %3 : $Vector):
// CHECK:   %4 = struct_extract %1 : ${{.*}}test1{{.*}}foo{{.*}}__Type{{.*}}, #{{.*}}.pv_0
// CHECK:   %5 = struct_extract %1 : ${{.*}}test1{{.*}}foo{{.*}}__Type{{.*}}, #{{.*}}.v_0
// CHECK:   %6 = alloc_stack $Vector
// CHECK:   %7 = begin_access [init] [static] [no_nested_conflict] %6 : $*Vector
// CHECK:   %8 = begin_access [init] [static] [no_nested_conflict] %7 : $*Vector
// CHECK:   store %3 to %8 : $*Vector
// CHECK:   end_access %8 : $*Vector
// CHECK:   end_access %7 : $*Vector
// CHECK:   %12 = begin_access [read] [static] [no_nested_conflict] %6 : $*Vector
// CHECK:   %13 = load %12 : $*Vector
// CHECK:   end_access %12 : $*Vector
// CHECK:   %15 = function_ref @{{.*}}fakeAdj{{.*}} : $@convention(thin) (Vector, Vector, Vector, Vector) -> (Vector, Vector)
// CHECK:   %16 = apply %15(%0, %0, %5, %13) : $@convention(thin) (Vector, Vector, Vector, Vector) -> (Vector, Vector)
// CHECK:   dealloc_stack %6 : $*Vector
// CHECK:   %18 = tuple_extract %16 : $(Vector, Vector), 0
// CHECK:   %19 = tuple_extract %16 : $(Vector, Vector), 1
// CHECK:   %20 = alloc_stack $Vector
// CHECK:   %21 = alloc_stack $Vector
// CHECK:   %22 = alloc_stack $Vector
// CHECK:   %23 = begin_access [init] [static] [no_nested_conflict] %21 : $*Vector
// CHECK:   %24 = begin_access [init] [static] [no_nested_conflict] %22 : $*Vector
// CHECK:   store %18 to %23 : $*Vector
// CHECK:   store %19 to %24 : $*Vector
// CHECK:   end_access %23 : $*Vector
// CHECK:   end_access %24 : $*Vector
// CHECK:   %29 = begin_access [init] [static] [no_nested_conflict] %20 : $*Vector
// CHECK:   %30 = begin_access [read] [static] [no_nested_conflict] %21 : $*Vector
// CHECK:   %31 = begin_access [read] [static] [no_nested_conflict] %22 : $*Vector
// CHECK:   %32 = witness_method $Vector, #VectorNumeric."+"!1 : <Self where Self : VectorNumeric> (Self.Type) -> (Self, Self) -> @dynamic_self Self : $@convention(witness_method: VectorNumeric) <τ_0_0 where τ_0_0 : VectorNumeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   %33 = metatype $@thick Vector.Type
// CHECK:   %34 = apply %32<Vector>(%29, %31, %30, %33) : $@convention(witness_method: VectorNumeric) <τ_0_0 where τ_0_0 : VectorNumeric> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   end_access %29 : $*Vector
// CHECK:   end_access %31 : $*Vector
// CHECK:   end_access %30 : $*Vector
// CHECK:   dealloc_stack %22 : $*Vector
// CHECK:   dealloc_stack %21 : $*Vector
// CHECK:   %40 = begin_access [read] [static] [no_nested_conflict] %20 : $*Vector
// CHECK:   %41 = load %40 : $*Vector
// CHECK:   end_access %40 : $*Vector
// CHECK:   dealloc_stack %20 : $*Vector
// CHECK:   return %41 : $Vector
// CHECK: }
