// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import IOKit
import IOKit.hid
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var IOKitTests = TestSuite("IOKit")

IOKitTests.test("IOReturn value") {
  // Error codes are computed by a helper function so it's enough to test one.
  // The value is taken from the OS X 10.11 SDK.
  expectEqual(Int(kIOReturnStillOpen), -536870190)
}

IOKitTests.test("IOReturn type") {
  let manager = IOHIDManagerCreate(nil, 0)!.takeRetainedValue()
  let result = IOHIDManagerClose(manager, 0)
  expectTrue(result.dynamicType == kIOReturnNotOpen.dynamicType)
}

runAllTests()
