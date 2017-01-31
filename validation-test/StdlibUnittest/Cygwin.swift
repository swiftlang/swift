// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

import Swift
import StdlibUnittest

_setOverrideOSVersion(.windowsCygnus)
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsWindows = TestSuite("XFailsCygwin")

// CHECK: [   UXPASS ] XFailsCygwin.xfail Cygwin passes{{$}}
XFailsWindows.test("xfail Cygwin passes").xfail(.windowsCygnusAny(reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsCygwin.xfail Cygwin fails{{$}}
XFailsWindows.test("xfail Cygwin fails").xfail(.windowsCygnusAny(reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: [   UXPASS ] XFailsCygwin.xfail Windows passes{{$}}
XFailsWindows.test("xfail Windows passes").xfail(.windowsAny(reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsCygwin.xfail Windows fails{{$}}
XFailsWindows.test("xfail Windows fails").xfail(.windowsAny(reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsCygwin: Some tests failed, aborting
// CHECK: abort()

runAllTests()

