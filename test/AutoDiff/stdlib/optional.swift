// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import _Differentiation
import StdlibUnittest

var OptionalDifferentiationTests = TestSuite("OptionalDifferentiation")

OptionalDifferentiationTests.test("Optional operations") {
  // Differentiable.move(by:)
  do {
    var some: Float? = 2
    some.move(by: .init(3))
    expectEqual(5, some)

    var none: Float? = nil
    none.move(by: .init(3))
    expectEqual(nil, none)
  }
}

OptionalDifferentiationTests.test("Optional.TangentVector operations") {
  // Differentiable.move(by:)
  do {
    var some: Optional<Float>.TangentVector = .init(2)
    some.move(by: .init(3))
    expectEqual(5, some.value)

    var none: Optional<Float>.TangentVector = .init(nil)
    none.move(by: .init(3))
    expectEqual(nil, none.value)

    var nestedSome: Optional<Optional<Float>>.TangentVector = .init(.init(2))
    nestedSome.move(by: .init(.init(3)))
    expectEqual(.init(5), nestedSome.value)

    var nestedNone: Optional<Optional<Float>>.TangentVector = .init(.init(nil))
    nestedNone.move(by: .init(.init(3)))
    expectEqual(.init(nil), nestedNone.value)
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

OptionalDifferentiationTests.test("Optional.TangentVector reflection") {
  let tan = Optional<Float>.TangentVector(42)
  let children = Array(Mirror(reflecting: tan).children)
  expectEqual(1, children.count)
  // We test `==` first because `as?` will flatten optionals.
  expectTrue(type(of: children[0].value) == Float.self)
  if let child = expectNotNil(children[0].value as? Float) {
    expectEqual(42, child)
  }
}

runAllTests()
