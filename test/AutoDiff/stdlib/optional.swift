// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import _Differentiation
import StdlibUnittest

var OptionalDifferentiationTests = TestSuite("OptionalDifferentiation")

OptionalDifferentiationTests.test("Optional operations") {
  // Differentiable.move(along:)
  do {
    var some: Float? = 2
    some.move(along: .init(3))
    expectEqual(5, some)

    var none: Float? = nil
    none.move(along: .init(3))
    expectEqual(nil, none)
  }

  // Differentiable.zeroTangentVectorInitializer
  do {
    let some: [Float]? = [1, 2, 3]
    expectEqual(.init([0, 0, 0]), some.zeroTangentVectorInitializer())

    let none: [Float]? = nil
    expectEqual(.init(nil), none.zeroTangentVectorInitializer())
  }
}

OptionalDifferentiationTests.test("Optional.TangentVector operations") {
  // Differentiable.move(along:)
  do {
    var some: Optional<Float>.TangentVector = .init(2)
    some.move(along: .init(3))
    expectEqual(5, some.value)

    var none: Optional<Float>.TangentVector = .init(nil)
    none.move(along: .init(3))
    expectEqual(nil, none.value)
  }

  // Differentiable.zeroTangentVectorInitializer
  do {
    var some: [Float]? = [1, 2, 3]
    expectEqual(.init([0, 0, 0]), some.zeroTangentVectorInitializer())

    var none: [Float]? = nil
    expectEqual(.init(nil), none.zeroTangentVectorInitializer())
  }

  // AdditiveArithmetic.zero
  expectEqual(.init(Float.zero), Float?.TangentVector.zero)
  expectEqual(.init([Float].TangentVector.zero), [Float]?.TangentVector.zero)

  // AdditiveArithmetic.+, AdditiveArithmetic.-
  do {
    var some: Optional<Float>.TangentVector = .init(2)
    var none: Optional<Float>.TangentVector = .init(nil)

    expectEqual(.init(4), some + some)
    expectEqual(.init(2), some + none)
    expectEqual(.init(2), none + some)
    expectEqual(.init(nil), none + none)

    expectEqual(.init(0), some - some)
    expectEqual(.init(2), some - none)
    expectEqual(.init(-2), none - some)
    expectEqual(.init(nil), none - none)
  }
}

runAllTests()
