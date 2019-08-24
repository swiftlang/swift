// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import IOKit
import IOKit.hid
import StdlibUnittest


var IOKitTests = TestSuite("IOKit")

IOKitTests.test("IOReturn value") {
  // Error codes are computed by a helper function so it's enough to test one.
  // The value is taken from the OS X 10.11 SDK.
  expectEqual(Int(kIOReturnStillOpen), -536870190)
}

IOKitTests.test("IOReturn type") {
  let manager = IOHIDManagerCreate(nil, 0)
  let result = IOHIDManagerClose(manager, 0)
  expectTrue(type(of: result) == type(of: kIOReturnNotOpen))
}

runAllTests()
