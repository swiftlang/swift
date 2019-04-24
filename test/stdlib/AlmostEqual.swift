// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var approximateTestSuite = TestSuite("AlmostEqual")

extension BinaryFloatingPoint
where RawSignificand: FixedWidthInteger, Exponent: FixedWidthInteger {

  static func testAlmostEqual() {
    
    // Values for testing:
    var values = (Self.leastNonzeroMagnitude.exponent ...
                  Self.greatestFiniteMagnitude.exponent).flatMap {
      exp in [
        Self(sign: .plus, exponent: exp, significand: 1).nextDown,
        Self(sign: .plus, exponent: exp, significand: 1),
        Self(sign: .plus, exponent: exp, significand: 1).nextUp,
        Self(sign: .plus, exponent: exp, significand: .random(in: 1..<2))
      ]
    }
    values.append(.infinity)
    
    // Tolerances for testing:
    let tolerances = [
      2*Self.ulpOfOne, .random(in: .ulpOfOne..<1), Self(1).nextDown
    ]
    
    // NaN is not almost equal to anything, with any tolerance.
    expectFalse(Self.nan.isAlmostEqual(to: .nan))
    for tol in tolerances {
      expectFalse(Self.nan.isAlmostEqual(to: .nan, tolerance: tol))
    }
    for val in values {
      expectFalse(Self.nan.isAlmostEqual(to: val))
      expectFalse(val.isAlmostEqual(to: .nan))
      for tol in tolerances {
        expectFalse(Self.nan.isAlmostEqual(to: val, tolerance: tol))
        expectFalse(val.isAlmostEqual(to: .nan, tolerance: tol))
      }
    }
    
    for val in values {
      expectTrue(val.isAlmostEqual(to: val))
      expectTrue(val.isAlmostEqual(to: val.nextUp))
      expectTrue(val.nextUp.isAlmostEqual(to: val))
      expectTrue(val.isAlmostEqual(to: val.nextDown))
      expectTrue(val.nextDown.isAlmostEqual(to: val))
      for tol in tolerances {
        expectTrue(val.isAlmostEqual(to: val, tolerance: tol))
        expectTrue(val.isAlmostEqual(to: val.nextUp, tolerance: tol))
        expectTrue(val.nextUp.isAlmostEqual(to: val, tolerance: tol))
        expectTrue(val.isAlmostEqual(to: val.nextDown, tolerance: tol))
        expectTrue(val.nextDown.isAlmostEqual(to: val, tolerance: tol))
      }
    }
  }
  
  static func testAlmostZero() {

    for val in [
      Self.ulpOfOne.squareRoot(), 1, .greatestFiniteMagnitude,
      .infinity, .nan
    ] {
      expectFalse(val.isAlmostZero())
      expectFalse((-val).isAlmostZero())
    }
    
    for val in [
      Self.ulpOfOne.squareRoot().nextDown, .leastNormalMagnitude,
      .leastNonzeroMagnitude, 0
    ] {
      expectTrue(val.isAlmostZero())
      expectTrue((-val).isAlmostZero())
    }
  }
}

approximateTestSuite.test("AlmostEqual") {
  Float.testAlmostEqual()
  Double.testAlmostEqual()
  Float.testAlmostZero()
  Double.testAlmostZero()
}

runAllTests()
