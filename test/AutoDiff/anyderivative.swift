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
  expectEqual(zero, zero.allDifferentiableVariables)

  var tan = AnyDerivative(Vector.TangentVector(x: 1, y: 1))
  expectEqual(zero, tan.tangentVector(from: zero))
  expectEqual(AnyDerivative(Vector.TangentVector.zero), tan - tan)
  expectNotEqual(AnyDerivative(Vector.TangentVector.zero), zero)
  expectNotEqual(AnyDerivative.zero, tan - tan)
  tan += zero
  tan -= zero
  expectEqual(tan, tan + zero)
  expectEqual(tan, tan - zero)
  expectEqual(tan, tan.moved(along: zero))
  expectEqual(tan, zero.moved(along: tan))
  expectEqual(zero, tan.tangentVector(from: zero))
  expectEqual(tan, zero.tangentVector(from: tan))
}

AnyDerivativeTests.test("Casting") {
  let tan = AnyDerivative(Vector.TangentVector(x: 1, y: 1))
  expectEqual(Vector.TangentVector(x: 1, y: 1), tan.base as? Vector.TangentVector)

  let genericTan = AnyDerivative(Generic<Float>.TangentVector(x: 1))
  expectEqual(Generic<Float>.TangentVector(x: 1),
              genericTan.base as? Generic<Float>.TangentVector)
  expectEqual(nil, genericTan.base as? Generic<Double>.TangentVector)

  let zero = AnyDerivative.zero
  expectEqual(nil, zero.base as? Float)
  expectEqual(nil, zero.base as? Vector.TangentVector)
  expectEqual(nil, zero.base as? Generic<Float>.TangentVector)
}

runAllTests()
