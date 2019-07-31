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
  @differentiable(vjp: vjpY)
  var y: Float {
    return .zero
  }

  func vjpY() -> (Float, (Float) -> Float) {
    return (.zero, { $0 })
  }

  // This should generate public symbols for both JVP and VJP.
  @differentiable
  init(x: Float) {
    self = x
  }

  // This should generate public symbols for both JVP and VJP.
  // Tests self-reordering-method thunking.
  @differentiable(jvp: jvpMethod)
  func method(x: Float, y: Float) -> Float {
    return x
  }
  func jvpMethod(x: Float, y: Float) -> (Float, (Float, Float, Float) -> Float) {
    return (x, { dself, dx, dy in dx })
  }

  // This should generate public symbols for both JVP and VJP.
  // Tests self-reordering-method thunking.
  @differentiable(vjp: vjpSubscript)
  subscript(x: Float) -> Float {
    return x
  }
  func vjpSubscript(x: Float) -> (Float, (Float) -> (Float, Float)) {
    return (x, { v in (0, v) })
  }
}

struct Nontrivial : Differentiable {
  var base: [Float]

  // This should generate public symbols for both JVP and VJP.
  // Tests differential/pullback thunking.
  @differentiable(jvp: jvpInit, vjp: vjpInit)
  init(_ base: [Float]) {
    self.base = base
  }
  static func jvpInit(_ base: [Float])
    -> (Nontrivial, (Array<Float>.TangentVector) -> Nontrivial.TangentVector) {
    return (Nontrivial(base), { v in Nontrivial.TangentVector(base: v) })
  }
  static func vjpInit(_ base: [Float])
    -> (Nontrivial, (Nontrivial.TangentVector) -> Array<Float>.TangentVector) {
    return (Nontrivial(base), { v in v.base })
  }

  // This should generate public symbols for both JVP and VJP.
  // Tests differential/pullback thunking.
  @differentiable(vjp: vjpOwnedParameterMismatch)
  func ownedParameter(_ x: __owned [Float]) -> [Float] {
    return x
  }
  func vjpOwnedParameterMismatch(_ x: __shared [Float])
    -> ([Float], (Array<Float>.TangentVector) -> (Nontrivial.TangentVector, Array<Float>.TangentVector)) {
    return (ownedParameter(x), { v in (.zero, v) })
  }

  // This should generate public symbols for both JVP and VJP.
  // Tests differential/pullback thunking.
  @differentiable(vjp: vjpSharedParameterMismatch)
  func sharedParameter(_ x: __shared [Float]) -> [Float] {
    return x
  }
  func vjpSharedParameterMismatch(_ x: __owned [Float])
    -> ([Float], (Array<Float>.TangentVector) -> (Nontrivial.TangentVector, Array<Float>.TangentVector)) {
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
