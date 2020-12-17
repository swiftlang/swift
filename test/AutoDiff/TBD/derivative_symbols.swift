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

  // Test property: getter and setter.
  public var property: Float {
    @differentiable
    get { stored }
    @differentiable
    set { stored = newValue }
  }

  // Test initializer.
  @differentiable
  public init(_ x: Float) {
    stored = x
  }

  // Test method.
  public func method(_ x: Float, _ y: Float) -> Float { x }

  @derivative(of: method)
  public func jvpMethod(_ x: Float, _ y: Float) -> (
    value: Float, differential: (TangentVector, Float, Float) -> Float
  ) {
    fatalError()
  }

  // Test subscript: getter and setter.
  public subscript(_ x: Float) -> Float {
    @differentiable
    get { x }

    @differentiable
    set { stored = newValue }
  }

  @derivative(of: subscript)
  public func vjpSubscript(_ x: Float) -> (
    value: Float, pullback: (Float) -> (TangentVector, Float)
  ) {
    fatalError()
  }

  @derivative(of: subscript.set)
  public mutating func vjpSubscriptSetter(_ x: Float, _ newValue: Float) -> (
    value: (), pullback: (inout TangentVector) -> (Float, Float)
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
