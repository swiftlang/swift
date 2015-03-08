// RUN: %target-run-stdlib-swift | FileCheck %s

import Swift
import StdlibUnittest

_setOverrideOSVersion(.Linux)
_setTestSuiteFailedCallback() { println("abort()") }

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

