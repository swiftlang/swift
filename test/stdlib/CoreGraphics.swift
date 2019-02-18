// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop


import CoreGraphics
import StdlibUnittest

let CoreGraphicsTests = TestSuite("CoreGraphics")

CoreGraphicsTests.test("CGColor/_ExpressibleByColorLiteral") {
  let color: CGColor = #colorLiteral(
    red: 0.5, green: 0.5, blue: 0.5, alpha: 1.0)
  // we primarilly care that the code above does not crash
  expectEqual([0.5, 0.5, 0.5, 1.0], color.components ?? [])
}

runAllTests()
