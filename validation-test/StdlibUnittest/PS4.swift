// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

import Swift
import StdlibUnittest

_setOverrideOSVersion(.ps4)
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsPS4 = TestSuite("XFailsPS4")

// CHECK: [   UXPASS ] XFailsPS4.xfail PS4 passes{{$}}
XFailsPS4.test("xfail PS4 passes").xfail(.ps4Any(reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsPS4.xfail PS4 fails{{$}}
XFailsPS4.test("xfail PS4 fails").xfail(.ps4Any(reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsPS4: Some tests failed, aborting
// CHECK: abort()

runAllTests()

