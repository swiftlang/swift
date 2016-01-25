// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import IOKit
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate

var IOKitTests = TestSuite("IOKit")

IOKitTests.test("IOReturn") {
  // Error codes are computed by a helper function so it's enough to test one.
  // The value is taken from the OS X 10.11 SDK.
  expectEqual(Int(kIOReturnStillOpen), -536870190)
}

runAllTests()
