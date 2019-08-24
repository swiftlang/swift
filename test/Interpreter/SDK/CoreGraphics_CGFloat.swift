// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// REQUIRES: rdar30317033

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
  expectEqual(4.125, CGFloat(CGFloat(Double(4.125))))

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
  let instances: [CGFloat] = [ 2.71, 3.14 ]

  checkHashable(instances, equalityOracle: { $0 == $1 })

  checkComparable(instances, oracle: { $0 <=> $1 })
}

CGFloatTestSuite.test("arithmetic") {
  let x: CGFloat = 0.25
  let y: CGFloat = 4
  let z: CGFloat = 0.5

  expectEqual(4.25, x + y)

  expectEqual(-3.75, x - y)

  expectEqual(1.0, x * y)

  expectEqual(0.0625, x / y)
}

CGFloatTestSuite.test("striding") {
  do {
    var result = [CGFloat]()
    for f in stride(from: (1.0 as CGFloat), to: 2.0, by: 0.5) {
      result.append(f)
    }
    expectEqual([ 1.0, 1.5 ], result)
  }

  do {
    var result = [CGFloat]()
    for f in stride(from: (1.0 as CGFloat), through: 2.0, by: 0.5) {
      result.append(f)
    }
    expectEqual([ 1.0, 1.5, 2.0 ], result)
  }
}

CGFloatTestSuite.test("bridging") {
  // Bridging to NSNumber.
  do {
    let flt: CGFloat = 4.125

    // CGFloat -> NSNumber conversion.
    let nsnum: NSNumber = flt as NSNumber
    expectEqual("4.125", "\(nsnum)")

    // NSNumber -> CGFloat
    let bridgedBack: CGFloat = nsnum as! CGFloat
    expectEqual(flt, bridgedBack)
  }

  // Array bridging.
  do {
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

CGFloatTestSuite.test("varargs") {
  let v: CVarArg = CGFloat(0)
  expectEqual(
    "0.023230", NSString(format: "%.6f", CGFloat(0.02323) as CVarArg))
  expectEqual(
    "0.123450", NSString(format: "%.6f", CGFloat(0.12345) as CVarArg))
  expectEqual(
    "1.234560", NSString(format: "%.6f", CGFloat(1.23456) as CVarArg))
}

runAllTests()

