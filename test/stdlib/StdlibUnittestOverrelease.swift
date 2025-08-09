// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values
// REQUIRES: foundation

import Foundation
import StdlibUnittest

var Suite = TestSuite("Overrelease")

Suite.test("doesntfail").xfail(.never).code { }

autoreleasepool {
    runAllTests()
}
