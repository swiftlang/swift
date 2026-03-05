// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var DefaultDerivativesTests = TestSuite("DefaultDerivatives")

protocol P1 {
  func requirementNoDefault(_ x: Float) -> Float
}

extension P1 where Self : Differentiable {
  @derivative(of: requirementNoDefault, wrt: x)
  func vjpRequirementNoDefault(_ x: Float) -> (value: Float, pullback: (Float) -> (Float)) {
    (value: requirementNoDefault(x), pullback: { _ in 42 })
  }
}

@differentiable(reverse, wrt: x where T: Differentiable)
func requirementNoDefaultPG<T : P1>(p : T, x : Float) -> Float {
  return p.requirementNoDefault(x);
}

DefaultDerivativesTests.testWithLeakChecking("RequirementNoDefault") {
  expectEqual(42.0, gradient(at: 100500) { requirementNoDefaultPG(p: S1(3), x: $0) })
}

struct S1: P1, Differentiable {
  var _s : Float;
  init(_ value : Int) {
    _s = Float(value)
  }
  func requirementNoDefault(_ x: Float) -> Float {
      12 * x
  }
}

@differentiable(reverse, wrt: x)
func requirementNoDefaultS1(p : S1, x : Float) -> Float {
  return p.requirementNoDefault(x);
}

DefaultDerivativesTests.testWithLeakChecking("RequirementNoDefaultStaticType") {
  // FIXME: Not yet supported: the derivative is not inherited
  // https://github.com/swiftlang/swift/issues/87705
  // expectEqual(42.0, gradient(at: 100500) { requirementNoDefaultS1(p: S1(4), x: $0) })
  expectEqual(12.0, gradient(at: 100500) { requirementNoDefaultS1(p: S1(4), x: $0) })
}

protocol P2 {
  func requirementWithDefault(_ x: Float) -> Float
}

extension P2 where Self : Differentiable {
  func requirementWithDefault(_ x: Float) -> Float {
      13 * x
  }

  @derivative(of: requirementWithDefault, wrt: x)
  func vjpRequirementWithDefault(_ x: Float) -> (value: Float, pullback: (Float) -> (Float)) {
    (value: requirementWithDefault(x), pullback: { _ in 43 })
  }
}

struct S2: P2, Differentiable {
  var _s : Float;
  init(_ value : Int) {
    _s = Float(value)
  }
}

struct S3: P2, Differentiable {
  var _s : Float;
  init(_ value : Int) {
    _s = Float(value)
  }
  func requirementWithDefault(_ x: Float) -> Float {
      14 * x
  }
}

@differentiable(reverse, wrt: x where T: Differentiable)
func requirementWithDefaultPG<T : P2>(p : T, x : Float) -> Float {
  return p.requirementWithDefault(x);
}

DefaultDerivativesTests.testWithLeakChecking("RequirementWithDefaultGeneric1") {
  expectEqual(43.0, gradient(at: 100500) { requirementWithDefaultPG(p: S2(5), x: $0) })
}

DefaultDerivativesTests.testWithLeakChecking("RequirementWithDefaultGeneric2") {
  expectEqual(43.0, gradient(at: 100500) { requirementWithDefaultPG(p: S3(6), x: $0) })    
}

@differentiable(reverse, wrt: x)
func requirementWithDefaultS2(p : S2, x : Float) -> Float {
  return p.requirementWithDefault(x);
}

DefaultDerivativesTests.testWithLeakChecking("RequirementWithDefaultStaticType1") {
  expectEqual(43.0, gradient(at: 100500) { requirementWithDefaultS2(p: S2(7), x: $0) })
}

@differentiable(reverse, wrt: x)
func requirementWithDefaultS3(p : S3, x : Float) -> Float {
  return p.requirementWithDefault(x);
}

DefaultDerivativesTests.testWithLeakChecking("RequirementWithDefaultStaticType2") {
  // FIXME: Not yet supported: the derivative is not inherited
  // https://github.com/swiftlang/swift/issues/87705
  // expectEqual(43.0, gradient(at: 100500) { requirementWithDefaultS3(p: S3(8), x: $0) })
  expectEqual(14.0, gradient(at: 100500) { requirementWithDefaultS3(p: S3(8), x: $0) })
}

runAllTests()
