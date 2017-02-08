// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Windows)
import Glibc
#else
import Darwin
#endif

//
// Test that harness correctly handles the case when there is no child process
// to terminate during shutdown because it crashed during test execution.
//

var TestSuiteChildCrashes = TestSuite("TestSuiteChildCrashes")

TestSuiteChildCrashes.test("passes") {
  expectCrashLater()
  fatalError("crash")
}

runAllTests()

