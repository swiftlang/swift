// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
  import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android) || os(Cygwin) || os(Haiku)
  import Glibc
#elseif os(Windows)
  import MSVCRT
#else
#error("Unsupported platform")
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

