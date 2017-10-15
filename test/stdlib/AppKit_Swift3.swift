// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift -swift-version 3 %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import AppKit
import StdlibUnittest
import StdlibUnittestFoundationExtras

let AppKitTests = TestSuite("AppKit_Swift3")

AppKitTests.test("NSEventMaskFromType") {
  let eventType: NSEventType = .keyDown
  let eventMask = NSEventMaskFromType(eventType)
  expectEqual(eventMask, .keyDown)
}

runAllTests()
