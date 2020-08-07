// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O

import _Differentiation

@differentiable
public func topLevelDifferentiable(_ x: Float, _ y: Float) -> Float { x }

public func topLevelHasDerivative<T: Differentiable>(_ x: T) -> T {
  x
}

@derivative(of: topLevelHasDerivative)
public func topLevelDerivative<T: Differentiable>(_ x: T) -> (
  value: T, pullback: (T.TangentVector) -> T.TangentVector
) {
  fatalError()
}

public struct Struct: Differentiable {
  var stored: Float

  // Test property.
  @differentiable
  public var property: Float {
    stored
  }

  // Test initializer.
  @differentiable
  public init(_ x: Float) {
    stored = x
  }

  // Test method.
  public func method(x: Float, y: Float) -> Float { x }

  @derivative(of: method)
  public func jvpMethod(x: Float, y: Float) -> (
    value: Float, differential: (TangentVector, Float, Float) -> Float
  ) {
    fatalError()
  }

  // Test subscript.
  public subscript(x: Float) -> Float { x }

  @derivative(of: subscript)
  public func vjpSubscript(x: Float) -> (
    value: Float, pullback: (Float) -> (TangentVector, Float)
  ) {
    fatalError()
  }
}

extension Array where Element == Struct {
  @differentiable
  public func sum() -> Float {
    return 0
  }
}
