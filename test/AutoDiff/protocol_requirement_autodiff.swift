// RUN: %target-run-simple-swift

import StdlibUnittest

var ProtocolRequirementAutodiffTests = TestSuite("ProtocolRequirementAutodiff")

protocol DiffReq : Differentiable {
  @differentiable(wrt: (self, x))
  func f(_ x: Float) -> Float
}

extension DiffReq where TangentVector : AdditiveArithmetic, CotangentVector : AdditiveArithmetic {
  func gradF(at x: Float) -> (Self.CotangentVector, Float) {
    return (valueWithPullback(at: x) { s, x in s.f(x) }).1(1)
  }
}

struct Quadratic : DiffReq, Equatable {
  typealias TangentVector = Quadratic
  typealias CotangentVector = Quadratic

  @differentiable(wrt: (self), vjp: vjpA)
  let a: Float
  func vjpA() -> (Float, (Float) -> Quadratic) {
    return (a, { da in Quadratic(da, 0, 0) } )
  }

  @differentiable(wrt: (self), vjp: vjpB)
  let b: Float
  func vjpB() -> (Float, (Float) -> Quadratic) {
    return (b, { db in Quadratic(0, db, 0) } )
  }

  @differentiable(wrt: (self), vjp: vjpC)
  let c: Float
  func vjpC() -> (Float, (Float) -> Quadratic) {
    return (c, { dc in Quadratic(0, 0, dc) } )
  }

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

extension Quadratic : VectorNumeric {
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

ProtocolRequirementAutodiffTests.test("Trivial") {
  expectEqual((Quadratic(0, 0, 1), 12), Quadratic(11, 12, 13).gradF(at: 0))
  expectEqual((Quadratic(1, 1, 1), 2 * 11 + 12),
              Quadratic(11, 12, 13).gradF(at: 1))
  expectEqual((Quadratic(4, 2, 1), 2 * 11 * 2 + 12),
              Quadratic(11, 12, 13).gradF(at: 2))
}

runAllTests()
