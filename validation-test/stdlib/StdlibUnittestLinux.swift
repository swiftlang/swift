// RUN: %target-run-stdlib-swift | FileCheck %s
// REQUIRES: executable_test

import Swift
import StdlibUnittest

_setOverrideOSVersion(.Linux)
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsLinux = TestSuite("XFailsLinux")

// CHECK: [   UXPASS ] XFailsLinux.xfail iOS passes{{$}}
XFailsLinux.test("xfail iOS passes").xfail(.LinuxAny(reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsLinux.xfail iOS fails{{$}}
XFailsLinux.test("xfail iOS fails").xfail(.LinuxAny(reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsLinux: Some tests failed, aborting
// CHECK: abort()

runAllTests()

