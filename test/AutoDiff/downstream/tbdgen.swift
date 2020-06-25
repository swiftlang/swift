// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=missing %s -enable-testing -O

// TODO: These tests are disabled because the pullback struct makes the TBDGen be different before/after SILGen.
// UN: %empty-directory(%t)
// UN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/typecheck.tbd
// UN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd
// UN: diff -u %t/typecheck.tbd %t/emit-ir.tbd

@differentiable public func publicDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(wrt: (x)) public func publicDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }

// Tests SILGen derivative "forwarding thunk" (no derivative reabstraction/self-reordering).
@differentiable
public func publicNoDerivativeReabstraction<T: Differentiable>(_ x: T) -> T { return x }
@derivative(of: publicNoDerivativeReabstraction)
public func publicNoDerivativeReabstractionVJP<T: Differentiable>(_ x: T) -> (value: T, pullback: (T.TangentVector) -> T.TangentVector) {
  return (x, { $0 })
}

@differentiable internal func internalDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(wrt: (x)) internal func internalDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }

@differentiable private func privateDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(wrt: (x)) private func privateDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }

public extension Float {
  // This should generate public symbols for both JVP and VJP.
  @differentiable
  var x: Float {
    return self
  }

  // This should generate public symbols for JVP but not VJP, because VJP is user-defined.
  @differentiable
  var y: Float {
    return .zero
  }

  @derivative(of: y)
  func vjpY() -> (value: Float, pullback: (Float) -> Float) {
    return (.zero, { $0 })
  }

  // This should generate public symbols for both JVP and VJP.
  @differentiable
  init(x: Float) {
    self = x
  }

  // This should generate public symbols for both JVP and VJP.
  // Tests self-reordering-method thunking.
  @differentiable
  func method(x: Float, y: Float) -> Float {
    return x
  }
  @derivative(of: method)
  func jvpMethod(x: Float, y: Float) -> (value: Float, differential: (Float, Float, Float) -> Float) {
    return (x, { dself, dx, dy in dx })
  }

  // This should generate public symbols for both JVP and VJP.
  // Tests self-reordering-method thunking.
  @differentiable
  subscript(x: Float) -> Float {
    return x
  }
  @derivative(of: subscript)
  func vjpSubscript(x: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
    return (x, { v in (0, v) })
  }
}

struct Nontrivial : Differentiable {
  var base: [Float]

  // This should generate public symbols for both JVP and VJP.
  // Tests differential/pullback thunking.
  @differentiable
  init(_ base: [Float]) {
    self.base = base
  }
  @derivative(of: init)
  static func jvpInit(_ base: [Float])
    -> (value: Nontrivial, differential: (Array<Float>.TangentVector) -> Nontrivial.TangentVector) {
    return (Nontrivial(base), { v in Nontrivial.TangentVector(base: v) })
  }
  @derivative(of: init)
  static func vjpInit(_ base: [Float])
    -> (value: Nontrivial, pullback: (Nontrivial.TangentVector) -> Array<Float>.TangentVector) {
    return (Nontrivial(base), { v in v.base })
  }

  // This should generate public symbols for both JVP and VJP.
  // Tests differential/pullback thunking.
  @differentiable
  func ownedParameter(_ x: __owned [Float]) -> [Float] {
    return x
  }
  @derivative(of: ownedParameter)
  func vjpOwnedParameterMismatch(_ x: __shared [Float])
    -> (value: [Float], pullback: (Array<Float>.TangentVector) -> (Nontrivial.TangentVector, Array<Float>.TangentVector)) {
    return (ownedParameter(x), { v in (.zero, v) })
  }

  // This should generate public symbols for both JVP and VJP.
  // Tests differential/pullback thunking.
  @differentiable
  func sharedParameter(_ x: __shared [Float]) -> [Float] {
    return x
  }
  @derivative(of: sharedParameter)
  func vjpSharedParameterMismatch(_ x: __owned [Float])
    -> (value: [Float], pullback: (Array<Float>.TangentVector) -> (Nontrivial.TangentVector, Array<Float>.TangentVector)) {
    return (sharedParameter(x), { v in (.zero, v) })
  }
}

public func publicDiffableIndirect(_ x: Float, _ y: Float) -> Float { return x }

internal func internalDiffableIndirect(_ x: Float, _ y: Float) -> Float { return x }

private func privateDiffableIndirect(_ x: Float, _ y: Float) -> Float { return x }

func invokeIndirect() {
  print(gradient(of: publicDiffableIndirect)(1, 2))
  print(gradient(of: internalDiffableIndirect)(1, 2))
  print(gradient(of: privateDiffableIndirect)(1, 2))
}

@inlinable
@differentiable
public func inlinableDifferentiable(_ x: Float) -> Float { x }
