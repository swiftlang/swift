// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

import Swift
import StdlibUnittest


_setOverrideOSVersion(.android)
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsAndroid = TestSuite("XFailsAndroid")

// CHECK: [   UXPASS ] XFailsAndroid.xfail Android passes{{$}}
XFailsAndroid.test("xfail Android passes").xfail(.androidAny(reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsAndroid.xfail Android fails{{$}}
XFailsAndroid.test("xfail Android fails").xfail(.androidAny(reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsAndroid: Some tests failed, aborting
// CHECK: abort()

runAllTests()
