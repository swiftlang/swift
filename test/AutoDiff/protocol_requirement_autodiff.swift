// RUN: %target-run-simple-swift

import StdlibUnittest

var ProtocolRequirementAutodiffTests = TestSuite("ProtocolRequirementAutodiff")

protocol DiffReq : Differentiable {
  @differentiable(wrt: (self, x))
  func f(_ x: Float) -> Float
}

extension DiffReq where TangentVector : AdditiveArithmetic {
  func gradF(at x: Float) -> (Self.TangentVector, Float) {
    return (valueWithPullback(at: x) { s, x in s.f(x) }).1(1)
  }
}

struct Quadratic : DiffReq, Equatable {
  typealias TangentVector = Quadratic

  @differentiable
  let a: Float

  @differentiable
  let b: Float

  @differentiable
  let c: Float

  init(_ a: Float, _ b: Float, _ c: Float) {
    self.a = a
    self.b = b
    self.c = c
  }

  @differentiable(wrt: (self, x))
  func f(_ x: Float) -> Float {
    return a * x * x + b * x + c
  }
}

extension Quadratic : VectorProtocol {
  static var zero: Quadratic { return Quadratic(0, 0, 0) }
  static func + (lhs: Quadratic, rhs: Quadratic) -> Quadratic {
    return Quadratic(lhs.a + rhs.a, lhs.b + rhs.b, lhs.c + rhs.c)
  }
  static func - (lhs: Quadratic, rhs: Quadratic) -> Quadratic {
  return Quadratic(lhs.a + rhs.a, lhs.b + rhs.b, lhs.c + rhs.c)
}
  typealias Scalar = Float
  static func * (lhs: Float, rhs: Quadratic) -> Quadratic {
    return Quadratic(lhs * rhs.a, lhs * rhs.b, lhs * rhs.c)
  }
}

// Test witness method SIL type computation.
protocol P : Differentiable {
  @differentiable(wrt: (x, y))
  func foo(_ x: Float, _ y: Double) -> Float
}
struct S : P {
  @differentiable(wrt: (x, y))
  func foo(_ x: Float, _ y: Double) -> Float {
    return x
  }
}

ProtocolRequirementAutodiffTests.test("Trivial") {
  expectEqual((Quadratic(0, 0, 1), 12), Quadratic(11, 12, 13).gradF(at: 0))
  expectEqual((Quadratic(1, 1, 1), 2 * 11 + 12),
              Quadratic(11, 12, 13).gradF(at: 1))
  expectEqual((Quadratic(4, 2, 1), 2 * 11 * 2 + 12),
              Quadratic(11, 12, 13).gradF(at: 2))
}

runAllTests()
