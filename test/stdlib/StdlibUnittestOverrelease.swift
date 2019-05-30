// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Foundation
import StdlibUnittest

var Suite = TestSuite("Overrelease")

Suite.test("doesntfail").xfail(.never).code { }

autoreleasepool {
    runAllTests()
}
