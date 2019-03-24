// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var AnyDerivativeTests = TestSuite("AnyDerivative")

struct Vector : Differentiable {
  let x, y: Float
}
struct Generic<T: Differentiable> : Differentiable {
  let x: T
}

AnyDerivativeTests.test("Vector") {
  var tan = AnyDerivative(Vector.TangentVector(x: 1, y: 1))
  tan += tan
  expectEqual(AnyDerivative(Vector.TangentVector(x: 2, y: 2)), tan)
  expectEqual(tan, tan.allDifferentiableVariables)
  expectEqual(AnyDerivative(Vector.TangentVector(x: 4, y: 4)), tan + tan)
  expectEqual(AnyDerivative(Vector.TangentVector(x: 0, y: 0)), tan - tan)
  expectEqual(AnyDerivative(Vector.TangentVector(x: 4, y: 4)), tan.moved(along: tan))
  expectEqual(AnyDerivative(Vector.TangentVector(x: 2, y: 2)), tan.tangentVector(from: tan))
}

AnyDerivativeTests.test("Generic") {
  var tan = AnyDerivative(Generic<Float>.TangentVector(x: 1))
  let cotan = AnyDerivative(Generic<Float>.CotangentVector(x: 1))
  tan += tan
  expectEqual(AnyDerivative(Generic<Float>.TangentVector(x: 2)), tan)
  expectEqual(tan, tan.allDifferentiableVariables)
  expectEqual(AnyDerivative(Generic<Float>.TangentVector(x: 4)), tan + tan)
  expectEqual(AnyDerivative(Generic<Float>.TangentVector(x: 0)), tan - tan)
  expectEqual(AnyDerivative(Generic<Float>.TangentVector(x: 4)), tan.moved(along: tan))
  expectEqual(AnyDerivative(Generic<Float>.TangentVector(x: 1)), tan.tangentVector(from: cotan))
}

AnyDerivativeTests.test("Zero") {
  var zero = AnyDerivative.zero
  zero += zero
  zero -= zero
  expectEqual(zero, zero + zero)
  expectEqual(zero, zero - zero)
  expectEqual(zero, zero.moved(along: zero))
  expectEqual(zero, zero.tangentVector(from: zero))
  expectEqual(zero, zero.allDifferentiableVariables)

  var tan = AnyDerivative(Vector.TangentVector(x: 1, y: 1))
  expectEqual(AnyDerivative(Vector.TangentVector.zero), zero)
  expectEqual(AnyDerivative(Vector.TangentVector.zero), tan - tan)
  expectEqual(AnyDerivative.zero, tan - tan)
  tan += zero
  tan -= zero
  expectEqual(tan, tan + zero)
  expectEqual(tan, tan - zero)
  expectEqual(tan, tan.moved(along: zero))
  expectEqual(tan, zero.moved(along: tan))
  expectEqual(zero, tan.tangentVector(from: zero))
  expectEqual(tan, zero.tangentVector(from: tan))
}

runAllTests()
