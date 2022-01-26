// RUN: %target-swift-frontend -emit-ir -o/dev/null -Xllvm -sil-disable-pass=cmo -parse-as-library -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -Xllvm -sil-disable-pass=cmo -parse-as-library -module-name test -validate-tbd-against-ir=all %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O

import _Differentiation

@differentiable(reverse)
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
    @differentiable(reverse)
    get { stored }
    @differentiable(reverse)
    set { stored = newValue }
  }

  // Test initializer.
  @differentiable(reverse)
  public init(_ x: Float) {
    stored = x.squareRoot()
  }

  // Test delegating initializer.
  @differentiable(reverse)
  public init(blah x: Float) {
    self.init(x)
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
    @differentiable(reverse)
    get { x }

    @differentiable(reverse)
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
  @differentiable(reverse)
  public func sum() -> Float {
    return 0
  }
}

// SR-13866: Dispatch thunks and method descriptor mangling.
public protocol P: Differentiable {
  @differentiable(reverse, wrt: self)
  @differentiable(reverse, wrt: (self, x))
  func method(_ x: Float) -> Float

  @differentiable(reverse, wrt: self)
  var property: Float { get set }

  @differentiable(reverse, wrt: self)
  @differentiable(reverse, wrt: (self, x))
  subscript(_ x: Float) -> Float { get set }
}

public final class Class: Differentiable {
  var stored: Float

  // Test initializer.
  // FIXME(rdar://74380324)
  // @differentiable(reverse)
  public init(_ x: Float) {
    stored = x
  }

  // Test delegating initializer.
  // FIXME(rdar://74380324)
  // @differentiable(reverse)
  // public convenience init(blah x: Float) {
  //   self.init(x)
  // }

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
    @differentiable(reverse)
    get { x }

    // FIXME(SR-13096)
    // @differentiable(reverse)
    // set { stored = newValue }
  }

  @derivative(of: subscript)
  public func vjpSubscript(_ x: Float) -> (
    value: Float, pullback: (Float) -> (TangentVector, Float)
  ) {
    fatalError()
  }

  // FIXME(SR-13096)
  // @derivative(of: subscript.set)
  // public func vjpSubscriptSetter(_ x: Float, _ newValue: Float) -> (
  //   value: (), pullback: (inout TangentVector) -> (Float, Float)
  // ) {
  //   fatalError()
  // }
}
