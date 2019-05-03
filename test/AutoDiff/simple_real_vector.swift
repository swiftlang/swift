// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

@_fixed_layout
public struct Vector : AdditiveArithmetic, VectorNumeric, Differentiable {
  public var x: Float
  public var y: Float

  public static var zero: Vector {
    return Vector(0)
  }

  public init(_ scalar: Float) {
    self.x = scalar
    self.y = scalar
  }

  @differentiable(vjp: fakeVJP)
  public static func + (lhs: Vector, rhs: Vector) -> Vector {
    abort()
  }

  @differentiable(vjp: fakeVJP)
  public static func - (lhs: Vector, rhs: Vector) -> Vector {
    abort()
  }

  public static func * (lhs: Float, rhs: Vector) -> Vector {
    abort()
  }

  public static func fakeVJP(lhs: Vector, rhs: Vector) -> (Vector, (Vector) -> (Vector, Vector)) {
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


// TF-189: `TF189` is a non-trivial type but `TF189.AllDifferentiableVariables` is trivial.
// Should pass verification.
@_fixed_layout
public class NonTrivial {}
@_fixed_layout
public struct TF189: Differentiable {
  @noDerivative public let x: Double
  @noDerivative public let nonTrivial: NonTrivial

  func foo(input: Vector) -> Vector {
    return self.pullback(at: input) { m, x in
      m.applied(to: x)
    }(.zero).1
  }

  func applied(to input: Vector) -> Vector {
    return input
  }
}
