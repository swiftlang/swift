// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

import Swift
import StdlibUnittest


_setOverrideOSVersion(.freeBSD)
_setTestSuiteFailedCallback() { print("abort()") }

var XFailsFreeBSD = TestSuite("XFailsFreeBSD")

// CHECK: [   UXPASS ] XFailsFreeBSD.xfail FreeBSD passes{{$}}
XFailsFreeBSD.test("xfail FreeBSD passes").xfail(.freeBSDAny(reason: "")).code {
  expectEqual(1, 1)
}

// CHECK: [    XFAIL ] XFailsFreeBSD.xfail FreeBSD fails{{$}}
XFailsFreeBSD.test("xfail FreeBSD fails").xfail(.freeBSDAny(reason: "")).code {
  expectEqual(1, 2)
}

// CHECK: XFailsFreeBSD: Some tests failed, aborting
// CHECK: abort()

runAllTests()

