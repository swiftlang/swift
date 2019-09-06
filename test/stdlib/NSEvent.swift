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

    if #available(macOS 9999, *) {
        // NSEvent.SpecialKey.deleteForward used to have the wrong rawValue in
        // macOS 10.15 and below. See https://github.com/apple/swift/pull/26853
        // (rdar://54725550).
        testSpecialKey(NSEvent.SpecialKey.deleteForward, rawValue: 0xF728)
    }
}

runAllTests()
