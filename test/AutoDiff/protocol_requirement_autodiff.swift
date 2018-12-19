// TODO: Change to normal %target-run-simple-swift when there are stdlib
// functions for differentiating methods.
// RUN: %target-run-simple-parse-stdlib-swift

import Swift
import StdlibUnittest

var ProtocolRequirementAutodiffTests = TestSuite("ProtocolRequirementAutodiff")

func pullback<T, U, R>(
  at x: (T, U), in f: @autodiff (T) -> (U) -> R
) -> (R.CotangentVector) -> (T.CotangentVector, U.CotangentVector)
  where T : Differentiable, U : Differentiable, R : Differentiable {
  return Builtin.autodiffApplyMethodVJP(f, x.0, x.1).1
}

protocol DiffReq : Differentiable {
  @differentiable(reverse, wrt: (self, .0))
  func f(_ x: Float) -> Float
}

extension DiffReq {
  func gradF(at x: Float) -> (Self.CotangentVector, Float) {
    return pullback(at: (self, x), in: Self.f)(1)
  }
}

struct Quadratic : DiffReq, Equatable {
  typealias TangentVector = Quadratic
  typealias CoangentVector = Quadratic
  func moved(toward q: Quadratic) -> Quadratic {
    return Quadratic(a + q.a, b + q.b, c + q.c)
  }

  let a, b, c: Float
  init(_ a: Float, _ b: Float, _ c: Float) {
    self.a = a
    self.b = b
    self.c = c
  }

  func f(_ x: Float) -> Float {
    return a * x * x + b * x + c
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
