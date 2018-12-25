// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

@_fixed_layout
public struct Vector : AdditiveArithmetic, VectorNumeric, Differentiable {
  public var x: Float
  public var y: Float

  public typealias TangentVector = Vector
  public typealias Scalar = Float

  public static var zero: Vector {
    return Vector(0)
  }

  public init(_ scalar: Float) {
    self.x = scalar
    self.y = scalar
  }

  @differentiable(adjoint: fakeAdj)
  public static func + (lhs: Vector, rhs: Vector) -> Vector {
    abort()
  }

  @differentiable(adjoint: fakeAdj)
  public static func - (lhs: Vector, rhs: Vector) -> Vector {
    abort()
  }

  public static func * (lhs: Float, rhs: Vector) -> Vector {
    abort()
  }

  public static func fakeAdj(seed: Vector, y: Vector, lhs: Vector, rhs: Vector) -> (Vector, Vector) {
    abort()
  }
}

// This exists to minimize generated SIL.
@inline(never) func abort() -> Never { fatalError() }

public func test1() -> Vector {
  func foo(_ x: Vector) -> Float {
    return (x + x).x
  }
  return gradient(at: Vector(10), in: foo)
}

// CHECK-LABEL: @{{.*}}test1{{.*}}
// CHECK: [[CLOSURE:%.*]] = function_ref @{{.*}}test1{{.*}}foo{{.*}} : $@convention(thin) (Vector) -> Float
// CHECK: [[CLOSURE_THICK:%.*]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) (Vector) -> Float to $@callee_guaranteed (Vector) -> Float
// CHECK: [[CLOSURE_THICK_NOESC:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_THICK]] : $@callee_guaranteed (Vector) -> Float to $@noescape @callee_guaranteed (Vector) -> Float
// CHECK: autodiff_function [wrt 0] [order 1] [[CLOSURE_THICK_NOESC]] : $@noescape @callee_guaranteed (Vector) -> Float
