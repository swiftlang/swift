// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

import Swift
import StdlibUnittest


_setOverrideOSVersion(.fuchsia)
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsFuchsia = TestSuite("XFailsFuchsia")

// CHECK: [   UXPASS ] XFailsFuchsia.xfail Fuchsia passes{{$}}
XFailsFuchsia.test("xfail Fuchsia passes").xfail(.fuchsiaAny(reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsFuchsia.xfail Fuchsia fails{{$}}
XFailsFuchsia.test("xfail Fuchsia fails").xfail(.fuchsiaAny(reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsFuchsia: Some tests failed, aborting
// CHECK: abort()

runAllTests()

