// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import AppKit
import StdlibUnittest
import StdlibUnittestFoundationExtras

var NSEventTests = TestSuite("NSEventTests")

func testSpecialKey(_ specialKey: NSEvent.SpecialKey, rawValue: Int) {
    expectEqual(specialKey.rawValue, rawValue)
    expectEqual(specialKey.unicodeScalar, Unicode.Scalar(rawValue))
}

NSEventTests.test("NSEvent.specialKey") {
    testSpecialKey(NSEvent.SpecialKey.upArrow, rawValue: 0xF700)
}

runAllTests()
