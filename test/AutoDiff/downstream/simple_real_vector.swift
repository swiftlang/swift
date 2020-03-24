// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

@frozen
public struct Vector : AdditiveArithmetic, Differentiable {
  public var x: Float
  public var y: Float

  public static var zero: Vector {
    return Vector(0)
  }

  public init(_ scalar: Float) {
    self.x = scalar
    self.y = scalar
  }

  @differentiable
  public static func + (lhs: Vector, rhs: Vector) -> Vector {
    abort()
  }

  @differentiable
  public static func - (lhs: Vector, rhs: Vector) -> Vector {
    abort()
  }

  public static func * (lhs: Float, rhs: Vector) -> Vector {
    abort()
  }

  @derivative(of: +)
  @derivative(of: -)
  public static func fakeVJP(lhs: Vector, rhs: Vector) -> (value: Vector, pullback: (Vector) -> (Vector, Vector)) {
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
// CHECK: [[CLOSURE_DIFF:%.*]] = differentiable_function [parameters 0] [[CLOSURE_THICK]] : $@callee_guaranteed (Vector) -> Float
// CHECK: [[CLOSURE_DIFF_NOESC:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_DIFF]] : $@differentiable @callee_guaranteed (Vector) -> Float to $@differentiable @noescape @callee_guaranteed (Vector) -> Float

// TF-189: `TF189` is a non-trivial type but `TF189.AllDifferentiableVariables` is trivial.
// Should pass verification.
@_fixed_layout
public class NonTrivial {}
@frozen
public struct TF189: Differentiable {
  @noDerivative public let x: Double
  @noDerivative public let nonTrivial: NonTrivial

  func foo(input: Vector) -> Vector {
    return pullback(at: self, input) { m, x in
      m.applied(to: x)
    }(.zero).1
  }

  func applied(to input: Vector) -> Vector {
    return input
  }
}
