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

// CHECK: sil hidden @AD__$s18simple_real_vector5TF189V3foo5inputAA6Vector{{.*}}___vjp_src_0_wrt_0_1 : $@convention(thin) (@guaranteed TF189, Vector) -> (Vector, @owned @callee_guaranteed (Vector) -> (TF189.AllDifferentiableVariables, Vector))
