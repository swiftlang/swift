// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

@_fixed_layout
public struct Vector : AdditiveArithmetic, VectorNumeric, Differentiable {
  public var x: Float
  public var y: Float

  public typealias TangentVector = Vector
  public typealias Scalar = Float
  public typealias Shape = ()

  public static var zero: Vector {
    return Vector(0)
  }

  public init(repeating repeatedValue: Float, shape: ()) {
    self.init(repeatedValue)
  }

  public init(_ scalar: Float) {
    self.x = scalar
    self.y = scalar
  }

  @differentiable(reverse, adjoint: fakeAdj)
  public static func + (lhs: Vector, rhs: Vector) -> Vector {
    abort()
  }

  @differentiable(reverse, adjoint: fakeAdj)
  public static func - (lhs: Vector, rhs: Vector) -> Vector {
    abort()
  }

  public static func * (lhs: Float, rhs: Vector) -> Vector {
    abort()
  }

  public static func fakeAdj(lhs: Vector, rhs: Vector, y: Vector, seed: Vector) -> (Vector, Vector) {
    abort()
  }
}

// This exists to minimize generated SIL.
@inline(never) func abort() -> Never { fatalError() }

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
// CHECK:   apply [[VECTOR_INIT_METHOD]]<Vector>(%1, %16, %15) : $@convention(witness_method: VectorNumeric) <τ_0_0 where τ_0_0 : VectorNumeric> (@in τ_0_0.Scalar, @thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[CAN_GRAD:%.*]] = function_ref @{{.*}}test1{{.*}}foo{{.*}}__grad_src_0_wrt_0_s_p : $@convention(thin) (Vector, Vector) -> (Vector, Vector)
// CHECK:   apply [[CAN_GRAD]]({{.*}}, {{.*}})
