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

    var nestedSome: Optional<Optional<Float>>.TangentVector = .init(.init(2))
    nestedSome.move(along: .init(.init(3)))
    expectEqual(.init(5), nestedSome.value)

    var nestedNone: Optional<Optional<Float>>.TangentVector = .init(.init(nil))
    nestedNone.move(along: .init(.init(3)))
    expectEqual(.init(nil), nestedNone.value)
  }

  // Differentiable.zeroTangentVectorInitializer
  do {
    let some: [Float]? = [1, 2, 3]
    expectEqual(.init([0, 0, 0]), some.zeroTangentVectorInitializer())

    let none: [Float]? = nil
    expectEqual(.init(nil), none.zeroTangentVectorInitializer())

    let nestedSome: [Float]?? = [1, 2, 3]
    expectEqual(.init(.init([0, 0, 0])), nestedSome.zeroTangentVectorInitializer())

    let nestedNone: [Float]?? = nil
    expectEqual(.init(nil), nestedNone.zeroTangentVectorInitializer())
  }

  // AdditiveArithmetic.zero
  expectEqual(.init(Float.zero), Float?.TangentVector.zero)
  expectEqual(.init([Float].TangentVector.zero), [Float]?.TangentVector.zero)

  expectEqual(.init(.init(Float.zero)), Float??.TangentVector.zero)
  expectEqual(.init(.init([Float].TangentVector.zero)), [Float]??.TangentVector.zero)

  // AdditiveArithmetic.+, AdditiveArithmetic.-
  do {
    let some: Optional<Float>.TangentVector = .init(2)
    let none: Optional<Float>.TangentVector = .init(nil)

    expectEqual(.init(4), some + some)
    expectEqual(.init(2), some + none)
    expectEqual(.init(2), none + some)
    expectEqual(.init(nil), none + none)

    expectEqual(.init(0), some - some)
    expectEqual(.init(2), some - none)
    expectEqual(.init(-2), none - some)
    expectEqual(.init(nil), none - none)

    let nestedSome: Optional<Optional<Float>>.TangentVector = .init(.init(2))
    let nestedNone: Optional<Optional<Float>>.TangentVector = .init(.init(nil))

    expectEqual(.init(.init(4)), nestedSome + nestedSome)
    expectEqual(.init(.init(2)), nestedSome + nestedNone)
    expectEqual(.init(.init(2)), nestedNone + nestedSome)
    expectEqual(.init(.init(nil)), nestedNone + nestedNone)

    expectEqual(.init(.init(0)), nestedSome - nestedSome)
    expectEqual(.init(.init(2)), nestedSome - nestedNone)
    expectEqual(.init(.init(-2)), nestedNone - nestedSome)
    expectEqual(.init(.init(nil)), nestedNone - nestedNone)
  }
}

runAllTests()
