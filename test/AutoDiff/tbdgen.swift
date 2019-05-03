// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -enable-testing
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -O
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -swift-version 4 %s -enable-testing -O

// TODO: These tests are disabled because the primal value struct makes the TBDGen be different before/after SILGen.
// UN: %empty-directory(%t)
// UN: %target-swift-frontend -typecheck -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/typecheck.tbd
// UN: %target-swift-frontend -emit-ir -parse-as-library -module-name test %s -emit-tbd -emit-tbd-path %t/emit-ir.tbd
// UN: diff -u %t/typecheck.tbd %t/emit-ir.tbd

@differentiable() public func publicDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(wrt: (x)) public func publicDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }

@differentiable() internal func internalDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(wrt: (x)) internal func internalDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }

@differentiable() private func privateDiffable(_ x: Float, _ y: Float) -> Float { return x }
@differentiable(wrt: (x)) private func privateDiffableWRT(_ x: Float, _ y: Float) -> Float { return x }

public extension Float {
  // This should generate public symbols for both JVP and VJP.
  @differentiable
  var x: Float {
    return .zero
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
  @differentiable
  subscript(x: Float) -> Float {
    return x
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
