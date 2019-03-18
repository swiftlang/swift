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
// Test that a test runs in its own child process if asked.
//

enum Globals {
  static var modifiedByChildProcess = false
}

var TestSuiteRequireNewProcess = TestSuite("TestSuiteRequireNewProcess")

TestSuiteRequireNewProcess.test("RequireOwnProcessBefore")
  .code {
  Globals.modifiedByChildProcess = true
}

TestSuiteRequireNewProcess.test("RequireOwnProcess")
  .requireOwnProcess()
  .code {
  expectEqual(false, Globals.modifiedByChildProcess)
  Globals.modifiedByChildProcess = true
}

TestSuiteRequireNewProcess.test("ShouldNotReusePreviousProcess") {
  expectEqual(false, Globals.modifiedByChildProcess)
}

runAllTests()
