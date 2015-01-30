// RUN: %target-run-simple-swift

// REQUIRES: objc_interop
// XFAIL: interpret

import CoreGraphics
import Foundation
import StdlibUnittest

var CGFloatTestSuite = TestSuite("CGFloat")

CGFloatTestSuite.test("literals") {
  var flt: CGFloat = 4.125
  expectEqual(4.125, flt)

  flt = 42
  expectEqual(42, flt)
}

CGFloatTestSuite.test("init") {
  expectEqual(0.0, CGFloat())
  expectEqual(4.125, CGFloat(Float(4.125)))
  expectEqual(4.125, CGFloat(Double(4.125)))

  expectEqual(42, CGFloat(Int(42)))
  expectEqual(42, CGFloat(Int8(42)))
  expectEqual(42, CGFloat(Int16(42)))
  expectEqual(42, CGFloat(Int32(42)))
  expectEqual(42, CGFloat(Int64(42)))
  expectEqual(42, CGFloat(UInt(42)))
  expectEqual(42, CGFloat(UInt8(42)))
  expectEqual(42, CGFloat(UInt16(42)))
  expectEqual(42, CGFloat(UInt32(42)))
  expectEqual(42, CGFloat(UInt64(42)))
}

CGFloatTestSuite.test("initOtherTypesFromCGFloat") {
  let flt: CGFloat = 4.125

  expectEqual(4.125, Float(flt))
  expectEqual(4.125, Double(flt))

  expectEqual(4, Int(flt))
  expectEqual(4, Int8(flt))
  expectEqual(4, Int16(flt))
  expectEqual(4, Int32(flt))
  expectEqual(4, Int64(flt))
  expectEqual(4, UInt(flt))
  expectEqual(4, UInt8(flt))
  expectEqual(4, UInt16(flt))
  expectEqual(4, UInt32(flt))
  expectEqual(4, UInt64(flt))
}

CGFloatTestSuite.test("comparisons") {
  let x = 3.14
  let y = 3.14
  let z = 2.71

  expectTrue(x == y)
  expectFalse(x != y)
  checkHashable(true, x, y)

  expectFalse(x == z)
  expectTrue(x != z)
  checkHashable(false, x, z)

  expectFalse(x < z)
  expectFalse(x <= z)
  expectTrue(x >= z)
  expectTrue(x > z)
  checkComparable(.GT, x, z)

  expectTrue(z < x)
  expectTrue(z <= x)
  expectFalse(z >= x)
  expectFalse(z > x)
  checkComparable(.LT, z, x)

  expectFalse(x < y)
  expectTrue(x <= y)
  expectTrue(x >= y)
  expectFalse(x > y)
  checkComparable(.EQ, x, y)
}


CGFloatTestSuite.test("arithmetic") {
  let x: CGFloat = 0.25
  let y: CGFloat = 4
  let z: CGFloat = 0.5

  expectEqual(4.25, x + y)

  expectEqual(-3.75, x - y)

  expectEqual(1.0, x * y)

  expectEqual(0.0625, x / y)

  expectEqual(0.25, x % z)
}

CGFloatTestSuite.test("striding") {
  if true {
    var result = [CGFloat]()
    for f: CGFloat in stride(from: 1.0, to: 2.0, by: 0.5) {
      result.append(f)
    }
    expectEqual([ 1.0, 1.5 ], result)
  }

  if true {
    var result = [CGFloat]()
    for f: CGFloat in stride(from: 1.0, through: 2.0, by: 0.5) {
      result.append(f)
    }
    expectEqual([ 1.0, 1.5, 2.0 ], result)
  }
}

CGFloatTestSuite.test("bridging") {
  // Bridging to NSNumber.
  if true {
    let flt: CGFloat = 4.125

    // CGFloat -> NSNumber conversion.
    let nsnum: NSNumber = flt as NSNumber
    expectEqual("4.125", "\(nsnum)")

    // NSNumber -> CGFloat
    let bridgedBack: CGFloat = nsnum as! CGFloat
    expectEqual(flt, bridgedBack)
  }

  // Array bridging.
  if true {
    let originalArray: [CGFloat] = [ 4.125, 10.625 ]

    // Array -> NSArray
    let nsarr: NSArray = originalArray as NSArray
    expectEqual(2, nsarr.count)
    expectEqual("4.125", "\(nsarr[0])")
    expectEqual("10.625", "\(nsarr[1])")

    // NSArray -> Array
    expectEqualSequence(originalArray, nsarr as! [CGFloat])
  }
}

runAllTests()

